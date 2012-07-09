-module(bridge).
-behaviour(gen_server).

%% TODO: Enforce style consistency.

%% Gen server API, which should be familiar to Erlang speakers.
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).
-export([cast/3]).

-export([connect/1, new/1]).
-export([on_disconnect/2, on_error/2, on_reconnect/2]).

-export([join_channel/3, join_channel/4, join_channel/5,
         publish_service/3, publish_service/4]).
-export([get_channel/2, get_service/2, get_service/3]).
-export([leave_channel/3, leave_channel/4, leave_service/3, leave_service/4]).

-export([context/1, get_client/2]).

-record(state,
        { opts		= [],
	  context	= undefined,
	  serializer,
	  event_supervisor
	}).

start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

new(Opts) -> start_link(Opts).

init(Options) ->
    Opts = Options ++
	[{log, 2},
	 {redirector, "http://redirector.getbridge.com"},
	 {secure_redirector, "https://redirector.getbridge.com"},
	 {secure, true}],
    {ok,
     #state{opts = Opts,
            serializer = bridge.serializer:start_link({Opts, self()}),
            event_supervisor = bridge.event:start_link()}}.

handle_call(_Request, _From, _State) ->
    {error, "Synchronous support not yet added."}.

handle_cast(_Request, _State) ->
    {error, "Asynchronous support not yet added."}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Ex: bridge:cast(get_service("auth"), {join, Args = [term()]}, BridgePid)
cast(Server, {Method, Args}, State) ->
    send_command('SEND', [{ref, Server ++ [Method]}, {args, Args}], State).

handle_info({Type, M}, State = #state{event_supervisor = Sup}) ->
    bridge.event:notify(Sup, {Type, M}),
    {noreply, State}.

on_error(Callback, State = #state{event_supervisor = Sup}) ->
    bridge.event:add_handler(Sup, {error, Callback}),
    {ok, State}.

on_disconnect(Callback, State = #state{event_supervisor = Sup}) ->
    bridge.event:add_handler(Sup, {disconnect, Callback}),
    {ok, State}.

on_reconnect(Callback, State = #state{event_supervisor = Sup}) ->
    bridge.event:add_handler(Sup, {reconnect, Callback}),
    {ok, State}.

connect(#state{serializer = S}) ->
    gen_server:cast(S, connect).

publish_service(SvcName, Handler, State) ->
    publish_service(SvcName, Handler, undefined, State).

publish_service(SvcName, Handler, Callback, State) ->
    send_command('JOINWORKERPOOL',
		 [{name, SvcName}, {handler, Handler},
		  {callback, Callback}],
		 State).

join_channel(ChName, Handler, State) ->
    join_channel(ChName, Handler, true, State).

join_channel(ChName, Handler, Writeable, State) when is_boolean(Writeable) ->
    join_channel(ChName, Handler, Writeable, undefined,
                 State);
join_channel(ChName, Handler, Callback, State) ->
    join_channel(ChName, Handler, true, Callback, State).

join_channel(ChName, Handler, Writeable, Callback,
             State) ->
    send_command('JOINCHANNEL',
		 [{name, ChName}, {handler, Handler},
		  {callback, Callback}, {writeable, Writeable}],
		 State).

leave_service(ChName, Handler, State) ->
    leave_service(ChName, Handler, undefined, State).

leave_service(ChName, Handler, Callback, State) ->
    send_command('LEAVEWORKERPOOL',
		 [{name, ChName}, {handler, Handler},
		  {callback, Callback}],
		 State).

leave_channel(ChName, Handler, State) ->
    leave_channel(ChName, Handler, undefined, State).

leave_channel(ChName, Handler, Callback, State) ->
    send_command('LEAVECHANNEL',
		 [{name, ChName}, {handler, Handler},
		  {callback, Callback}],
		 State).

get_service(SvcName, _State) ->
    ["named", SvcName, SvcName].

get_service(SvcName, Client, _State) ->
    Client ++ [SvcName].

get_channel(ChName, State) ->
    send_command('GETCHANNEL', [{name, ChName}], State),
    ["channel", ChName, "channel:" ++ ChName].

context(#state{context = Context}) -> Context.

get_client(ClientId, _State) ->
    bridge.client:new(ClientId).

send_command(Op, Args, _State = #state{serializer = S}) ->
    gen_server:cast(S, {encode, Op, Args}).
