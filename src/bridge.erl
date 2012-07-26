-module(bridge).

-export_type([proplist/2, json_key/0, json_obj/0, json/0]).
-export_type([command/0, remote_service/0, service/0, options/0]).

-type proplist(K, V) :: [{K, V}].

-type json_key() :: atom()
                  | binary().
-type json_obj() :: {proplist(json_key(), json())}.
-type json() :: json_obj()
              | [json()]
              | json_key()
              | number().

-type command() :: 'JOINWORKERPOOL'
                 | 'JOINCHANNEL'
                 | 'LEAVEWORKERPOOL'
                 | 'LEAVECHANNEL'
                 | 'GETCHANNEL'
                 | 'CONNECT'
                 | 'SEND'.
-type remote_service() :: {[{binary(), [json_key()]}]}.
-type service() :: function()
                 | pid()
                 | remote_service()
                 | undefined.
-type options() :: proplist(atom(), any()).
-define(Ref(X), {[{<<"ref">>, X}]}).


%% Bridge API.
-export([publish_service/2, join_channel/2]).
-export([get_service/2, get_channel/2]).
-export([leave_service/2, leave_channel/2]).

-export([context/1, get_client/2]).

-export([new/1, connect/1]).
-export([add_handler/3]).
-export([cast/2]).

-spec new(options()) -> {ok, pid()} | {error, _}.
new(Opts) -> bridge_core:start_link(Opts).

-spec connect(pid()) -> ok.
connect(Pid) ->
    gen_server:cast(Pid, connect).

-spec is_service(service()) -> true.
is_service(?Ref(Lst)) when is_list(Lst) ->
    lists:all(fun(X) -> is_binary(X) orelse is_atom(X) end, Lst);
is_service(Term) ->
    is_function(Term) orelse is_pid(Term) orelse Term =:= undefined.

%% Ex: bridge:cast(BridgePid, {get_service(auth), join, Args = [term()]})
-spec cast(pid(), {service(), atom(), [json()]}) -> ok.
cast(Pid, {<<"undefined">>, _Method, _Args}) when is_pid(Pid)->
    ok;
cast(_Pid, {Svc, _Method, Args}) when is_function(Svc) andalso is_pid(_Pid) ->
    apply(Svc, Args),
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
                                              is_atom(SvcName) ->
    publish_service(Pid, {SvcName, Handler, undefined});
publish_service(Pid, {SvcName, Handler, Callback}) when is_pid(Pid) andalso
                                                        is_atom(SvcName) ->
    true = is_service(Handler) andalso is_service(Callback),
    send_command(Pid, 'JOINWORKERPOOL',
                 {[{name,     SvcName},
                   {handler,  Handler},
                   {callback, Callback}]}).

-spec join_channel(pid(), {atom(), service()}) ->                       ok;
                  (pid(), {atom(), service(), boolean()}) ->            ok;
                  (pid(), {atom(), service(), service()}) ->            ok;
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
get_service(_Bridge, {Client, SvcName}) when SvcName =/= system ->
    append_ref(Client, SvcName);
get_service(_Bridge, SvcName) when SvcName =/= system ->
    ?Ref([named, SvcName, SvcName]).

-spec get_channel(pid(), atom()) -> remote_service().
get_channel(Bridge, ChannelName) when is_pid(Bridge) andalso
                                      is_atom(ChannelName)   ->
    send_command(Bridge, 'GETCHANNEL', {[{name, ChannelName}]}),
    BinName = atom_to_binary(ChannelName, utf8),
    ?Ref([channel, ChannelName, <<"channel:", BinName/binary>>]).

-spec context(pid()) -> remote_service().
context(Bridge) ->
    gen_server:call(Bridge, context).

-spec get_client(pid(), json_key()) -> remote_service().
get_client(_Pid, ClientId) ->
    ?Ref([client, ClientId]).

-spec send_command(pid(), command(), json_obj()) -> ok.
send_command(Pid, Op, Args) ->
    gen_server:cast(Pid, {outbound, {Op, Args}}).


-spec append_ref(remote_service(), json_key()) -> remote_service().
append_ref({[{<<"ref">>, Ref} | _Tail]}, Element) ->
    ?Ref(Ref ++ [Element]);
append_ref({[_X | Next]}, Element) ->
    append_ref({Next}, Element).

