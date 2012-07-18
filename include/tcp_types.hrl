-include("gen_types.hrl").

-type socket() :: pid() | {sslsocket, new_ssl, pid()}.
