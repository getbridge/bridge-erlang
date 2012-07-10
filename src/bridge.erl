-module(bridge).
-behaviour(gen_server).

%% TODO: Enforce style consistency.

%% Gen server API, which should be familiar to Erlang speakers.
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).
-export([cast/2, call/2]).

-export([new/1]).

-record(state,
        { opts		= [],
	  context	= undefined,
	  event_handler = undefined,
	  client_id     = undefined,
	  secret        = undefined,
	  api_key,
	  serializer
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
    Key = proplists:get_value(api_key, Opts),
    {ok, S} = bridge.serializer:start_link(Opts),
    {ok, #state{opts = Opts, serializer = S, api_key = Key}}.

call(Pid, {Method, Args}) ->
    gen_server:call(Pid, {Method, Args}).

handle_call({Method, Args}, _From, State) when is_atom(Method) ->
    {reply, apply(bridge, Method, Args ++ [State]), State};
handle_call(_Request, _From, _State) -> % Used for exposing API.
    {error, "No such method."}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Ex: bridge:cast(BridgePid, {get_service("auth"), join, Args = [term()]})
cast(Pid, {Svc, Method, Args}) ->
    Ref = Svc ++ [Method],
    gen_server:cast(Pid, {outbound,{'SEND', [{ref, Ref}, {args, Args}]}});
cast(Pid, connect) ->
    gen_server:cast(Pid, connect).

handle_cast({outbound, {'SEND', Data}}, _State) ->
    send_command('SEND', Data, _State),
    {noreply, _State};
handle_cast({connect_response, {Id, Secret}}, _State) ->
    {noreply, _State#state{client_id = Id, secret = Secret}};
handle_cast(connect, State = #state{serializer = S, api_key = Key,
				    client_id = Id, secret = Secret}) ->
    gen_server:cast(S, connect),
    {noreply, State}.

send_command(Op, Args, _State = #state{serializer = S}) ->
    gen_server:cast(S, {encode, Op, Args}).

handle_info(_Info, State = #state{event_handler = undefined}) ->
    {noreply, State};
handle_info(Info, State = #state{event_handler = E}) ->
    gen_event:notify(E, Info),
    {noreply, State}.

add_handler(State = #state{event_handler = E}) ->
    {ok, Ev} = gen_server:start(E),
    {ok, State#state{event_handler = Ev}}.

publish_service(SvcName, Handler, State) ->
    publish_service(SvcName, Handler, undefined, State).
publish_service(SvcName, Handler, Callback, State) ->
    send_command('JOINWORKERPOOL',
		 [{name, SvcName}, {handler, Handler}, {callback, Callback}],
		 State).

join_channel(Ch, Handler, State) ->
    join_channel(Ch, Handler, true, State).
join_channel(Ch, Handler, Writeable, State) when is_boolean(Writeable) ->
    join_channel(Ch, Handler, Writeable, undefined, State);
join_channel(Ch, Handler, Callback, State) ->
    join_channel(Ch, Handler, true, Callback, State).
join_channel(Ch, Handler, Writeable, Callback, State) ->
    send_command('JOINCHANNEL',
		 [{name, Ch}, {handler, Handler},
		  {callback, Callback}, {writeable, Writeable}],
		 State).

leave_service(Ch, Handler, State) ->
    leave_service(Ch, Handler, undefined, State).
leave_service(Ch, Handler, Callback, State) ->
    send_command('LEAVEWORKERPOOL',
		 [{name, Ch}, {handler, Handler},
		  {callback, Callback}],
		 State).
leave_channel(Ch, Handler, State) ->
    leave_channel(Ch, Handler, undefined, State).
leave_channel(Ch, Handler, Callback, State) ->
    send_command('LEAVECHANNEL',
		 [{name, Ch}, {handler, Handler},
		  {callback, Callback}],
		 State).

get_service(SvcName, _State) ->
    ["named", SvcName, SvcName].
get_service(SvcName, Client, _State) ->
    Client ++ [SvcName].

get_channel(Ch, State) ->
    send_command('GETCHANNEL', [{name, Ch}], State),
    ["channel", Ch, "channel:" ++ Ch].

context(#state{context = Context}) -> Context.

get_client(ClientId, _State) ->
    ["client", ClientId].
