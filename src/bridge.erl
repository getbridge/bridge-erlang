-module(bridge).

-include("bridge_types.hrl").

%% Bridge API.
-export([publish_service/2, join_channel/2]).
-export([get_service/2, get_channel/2]).
-export([leave_service/2, leave_channel/2]).

-export([context/1, get_client/2]).

-export([new/1, connect/1]).
-export([add_handler/3]).
-export([cast/2]).

-spec new(options()) -> {ok, pid()} | {error, _}.
new(Opts) -> bridge.core:start_link(Opts).

-spec connect(pid()) -> ok.
connect(Pid) ->
    gen_server:cast(Pid, connect).

-spec is_service(service()) -> true.
is_service({[ref, Lst]}) when is_list(Lst) ->
    lists:all(fun(X) -> is_binary(X) orelse is_atom(X) end, Lst);
is_service(Term) ->
    is_function(Term) orelse is_pid(Term) orelse Term =:= undefined.

%% Ex: bridge:cast(BridgePid, {get_service(auth), join, Args = [term()]})
-spec cast(pid(), {service(), atom(), [json()]}) -> ok.
cast(_Pid, {Svc, _Method, Args}) when is_function(Svc) andalso is_pid(_Pid) ->
    erlang:apply(Svc, Args ++ [_Pid]),
    ok;
cast(_Pid, {Svc, Method, Args}) when is_pid(Svc) andalso is_pid(_Pid) ->
    gen_server:cast(Svc, {Method, Args});
cast(Pid, {Svc, Method, Args}) when is_pid(Pid) ->
    Ref = append_ref(Svc, Method),
    send_command(Pid, 'SEND', {[{destination, Ref}, {args, Args}]}).

-spec add_handler(pid(), atom(), any()) -> ok.
add_handler(Pid, E, Args) when is_pid(Pid) andalso is_atom(E) ->
    {ok, Manager} = gen_event:start({local, bridge}),
    gen_event:add_handler(Manager, E, Args),
    ok = gen_server:cast(Pid, {add_handler, Manager}).

%% Handler is some process ID that implements the gen_server API. Method
%% invocation should be handled via
%%     handle_cast({method_name, Args = [term()] ++ [BridgePid]}, State)
-spec publish_service(pid(), {atom(), service()})            -> ok;
		     (pid(), {atom(), service(), service()}) -> ok.
publish_service(Pid, {SvcName, Handler}) when is_pid(Pid) andalso
					      is_atom(SvcName)    ->
    true = is_service(Handler),
    publish_service(Pid, {SvcName, Handler, undefined});
publish_service(Pid, {SvcName, Handler, Callback}) when is_pid(Pid) andalso
							is_atom(SvcName) ->
    true = is_service(Handler) andalso is_service(Callback),
    send_command(Pid, 'JOINWORKERPOOL',
                 {[{name,     SvcName},
                   {handler,  Handler},
                   {callback, Callback}]}).

-spec join_channel(pid(), {atom(), service()})                       -> ok;
                  (pid(), {atom(), service(), boolean()})            -> ok;
                  (pid(), {atom(), service(), service()})            -> ok;
                  (pid(), {atom(), service(), boolean(), service()}) -> ok.
join_channel(Pid, {ChannelName, Handler}) ->
    join_channel(Pid, {ChannelName, Handler, true, undefined});
join_channel(Pid, {ChannelName, Handler, Write}) when is_boolean(Write) ->
    join_channel(Pid, {ChannelName, Handler, Write, undefined});
join_channel(Pid, {ChannelName, Handler, Callback}) ->
    join_channel(Pid, {ChannelName, Handler, true, Callback});
join_channel(Pid, {ChannelName, Handler, Write, Callback}) ->
    send_command(Pid, 'JOINCHANNEL',
                 {[{name, ChannelName},
                   {handler, Handler},
                   {callback, Callback},
                   {writeable, Write}]}).

-spec leave_service(pid(), {atom(), service()})            -> ok;
		   (pid(), {atom(), service(), service()}) -> ok.
leave_service(Pid, {SvcName, Handler}) ->
    leave_service(Pid, {SvcName, Handler, undefined});
leave_service(Pid, {SvcName, Handler, Callback}) ->
    send_command(Pid, 'LEAVEWORKERPOOL',
                 {[{name, SvcName},
                   {handler, Handler},
                   {callback, Callback}]}).

-spec leave_channel(pid(), {atom(), service()})            -> ok;
		   (pid(), {atom(), service(), service()}) -> ok.
leave_channel(Pid, {ChannelName, Handler}) ->
    leave_channel(Pid, {ChannelName, Handler, undefined});
leave_channel(Pid, {ChannelName, Handler, Callback}) ->
    send_command(Pid, 'LEAVECHANNEL',
                 {[{name, ChannelName},
                   {handler, Handler},
                   {callback, Callback}]}).

%% Service name is provided as an atom, probably.
-spec get_service(pid(), atom())                     -> remote_service();
		 (pid(), {remote_service(), atom()}) -> remote_service().
get_service(_Bridge, SvcName) when SvcName =/= system ->
    create_ref([named, SvcName, SvcName]);
get_service(_Bridge, {Client, SvcName}) when SvcName =/= system ->
    append_ref(Client, SvcName).

-spec get_channel(pid(), atom()) -> remote_service().
get_channel(Bridge, ChannelName) when is_pid(Bridge) andalso
				      is_atom(ChannelName)   ->
    send_command(Bridge, 'GETCHANNEL', {[{name, ChannelName}]}),
    BinName = erlang:atom_to_binary(ChannelName, utf8),
    create_ref([channel, ChannelName, <<"channel:", BinName/binary>>]).

-spec context(pid()) -> remote_service().
context(Bridge) ->
    gen_server:call(Bridge, context).

-spec get_client(pid(), json_key()) -> remote_service().
get_client(_Pid, ClientId) ->
    create_ref([client, ClientId]).

-spec send_command(pid(), bridge_command(), json_obj()) -> ok.
send_command(Pid, Op, Args) ->
    gen_server:cast(Pid, {outbound, {Op, Args}}).


-spec append_ref(remote_service(), json_key()) -> remote_service().
append_ref({[{ref, Ref}]}, Element) ->
    create_ref(Ref ++ [Element]).

-spec create_ref([json_key()]) -> remote_service().
create_ref(Lst) ->
    {[{ref, Lst}]}.
