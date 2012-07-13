-module(bridge.core).
-behaviour(gen_server).

%% TODO: Enforce style consistency.

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, start_link/1, terminate/2]).
-export([cast/2, call/2]).

-import(gen_server).
-import(proplists).
-import(lists).

-record(state,
        { opts          = [],
	  buffer        = [],
          context       = undefined,
          event_handler = undefined,
          client_id     = null,
          secret        = null,
          connected     = false,
          api_key,
          serializer
        }).

start_link(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

init(Options) ->
    Opts = Options ++
        [{log, 2},
         {redirector, "http://redirector.getbridge.com"},
         {secure_redirector, "https://redirector.getbridge.com"},
         {secure, true}],
    Key = proplists:get_value(api_key, Opts),
    {ok, S} = bridge.serializer:start_link(Opts),
    connect(#state{opts = Opts, serializer = S, api_key = Key}).

call(Pid, {Method, Args}) when is_atom(Method) ->
    gen_server:call(Pid, {Method, Args}).

handle_call({publish_service, Args}, _From, State) ->
    {reply, apply(bridge, publish_service, Args ++ [State]), State};
handle_call(context, _From, State) ->
    {reply, context(State), State};
handle_call(_Stuff, _From, State) ->
    {reply, ok, State}.

connect(State = #state{api_key=Key, client_id = Id,
                       secret=Secret, serializer=S}) ->
    gen_server:cast(S, {connect, {[{command, 'CONNECT'},
				   {data, {[{api_key, Key},
					    {session, [Id, Secret]}]}
				   }]}
		       }),
    {ok, State}.

%% Ex: bridge:cast(BridgePid, {get_service("auth"), join, Args = [term()]})
cast(Pid, {Svc, Method, Args}) ->
    Ref = Svc ++ [Method],
    gen_server:cast(Pid, {outbound,{'SEND', [{ref, Ref}, {args, Args}]}}).


handle_cast({outbound, {Op, Data}}, _State) ->
    NewState = send_command(Op, Data, _State),
    {noreply, NewState};
handle_cast({add_handler, Mod}, _State) ->
    {ok, NewState} = add_handler(Mod, _State),
    {noreply, NewState};
handle_cast({[{destination, Dest} | Args]}, _State)
  when is_function(Dest) ->
    apply(Dest, Args ++ [self()]),
    {noreply, _State};
handle_cast({[{destination, Dest} | Args]}, _State) ->
    gen_server:cast(Dest, Args),
    {noreply, _State};
handle_cast({connect_response, {Id, Secret}}, _State = #state{buffer = Buf}) ->
    %% Flush queue.
    .io:format("Flushing ~p entries~n", [length(Buf)]),
    [Fun() || Fun <- lists:reverse(Buf)],
    {noreply, _State#state{client_id = Id, secret = Secret,
			   connected = true, buffer = []}};
handle_cast(connect, State) ->
    connect(State),
    {noreply, State}.


handle_info(_Info, State = #state{event_handler = undefined}) ->
    {noreply, State};
handle_info(Info, State = #state{event_handler = E}) ->
    gen_event:notify(E, Info),
    {noreply, State}.


add_handler(E, State = #state{}) ->
    {ok, Ev} = gen_server:start(E),
    {ok, State#state{event_handler = Ev}}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_command(Op, Args, State = #state{serializer = S,
				       connected = false,
				       buffer = Buf}) ->
    State#state{buffer=[fun() -> gen_server:cast(S, {encode, {Op, Args}}) end
			| Buf]};
send_command(Op, Args, State = #state{serializer = S}) ->
    gen_server:cast(S, {encode, Op, Args}),
    State.

context(#state{context = Context}) ->
    Context.
