-module(bridge).

%% TODO: Enforce style consistency.

%% Bridge API.
-export([publish_service/2, join_channel/2]).
-export([get_service/2, get_channel/2]).
-export([leave_service/2, leave_channel/2]).

-export([context/1, get_client/2]).

-export([new/1, connect/1]).
-export([add_handler/2]).
-export([cast/2]).

new(Opts) -> bridge.core:start_link(Opts).

connect(Pid) ->
    gen_server:cast(Pid, connect).

-spec bridge:create_ref([any()]) -> {[{ref, [any()]}]}.
create_ref(Lst) ->
    {[{ref, Lst}]}.

%% Ex: bridge:cast(BridgePid, {get_service(auth), join, Args = [term()]})
-spec bridge:cast(pid(), {atom(), atom(), [any()]}) -> ok.
cast(Pid, {Svc, Method, Args}) ->
    Ref = create_ref(Svc ++ [Method]),
    send_command(Pid, 'SEND', {[{destination, Ref}, {args, Args}]}).

-spec bridge:add_handler(pid(), atom) -> ok.
add_handler(Pid, E) ->
    {ok, Ev} = gen_server:start(E),
    ok = gen_server:cast(Pid, {add_handler, Ev}).

%% Handler is some process ID that implements the gen_server API. Method
%% invocation should be handled via
%%        handle_cast({method_name, Args = [term()] ++ [BridgePid]}, State)
-spec bridge:publish_service(pid(), {any()}) -> ok.
publish_service(Pid, {SvcName, Handler}) ->
    publish_service(Pid, {SvcName, Handler, undefined});
publish_service(Pid, {SvcName, Handler, Callback}) ->
    send_command(Pid, 'JOINWORKERPOOL',
                 {[{name, SvcName},
                   {handler, Handler},
                   {callback, Callback}]}).

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

leave_service(Pid, {SvcName, Handler}) ->
    leave_service(Pid, {SvcName, Handler, undefined});
leave_service(Pid, {SvcName, Handler, Callback}) ->
    send_command(Pid, 'LEAVEWORKERPOOL',
                 {[{name, SvcName},
                   {handler, Handler},
                   {callback, Callback}]}).

leave_channel(Pid, {ChannelName, Handler}) ->
    leave_channel(Pid, {ChannelName, Handler, undefined});
leave_channel(Pid, {ChannelName, Handler, Callback}) ->
    send_command(Pid, 'LEAVECHANNEL',
                 {[{name, ChannelName},
                   {handler, Handler},
                   {callback, Callback}]}).

%% Service name is provided as an atom, probably.
get_service(_Bridge, SvcName) when SvcName =/= system ->
    [named, SvcName, SvcName];
get_service(_Bridge, {Client, SvcName}) when SvcName =/= system ->
    Client ++ [SvcName].

get_channel(Bridge, ChannelName) ->
    send_command(Bridge, 'GETCHANNEL', {[{name, ChannelName}]}),
    [channel, ChannelName, <<"channel:", ChannelName/binary>>].

context(Bridge) ->
    gen_server:call(Bridge, context).

get_client(_Pid, ClientId) ->
    [client, ClientId].

send_command(Pid, Op, Args) ->
    gen_server:cast(Pid, {outbound, {Op, Args}}).
