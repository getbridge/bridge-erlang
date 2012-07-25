-include("gen_types.hrl").

-type address() :: ip_address() | hostname().
-type ip_address() :: ip4_address() | ip6_address().
-type ip4_address() :: { byte(), byte(), byte(), byte() }.

-type ip6_address() :: { 0..65535, 0..65535, 0..65535, 0..65535, 
                         0..65535, 0..65535, 0..65535, 0..65535  }.

-type httpc_result() :: {status_line(), headers(), httpc_body()}
                      | {status_code(), httpc_body()}.

-type httpc_body() :: string() | binary().

-type status_line()    :: {http_version(), status_code(), reason_phrase()}.
-type http_version()   :: string().
-type status_code()    :: pos_integer().
-type reason_phrase()  :: string().
-type content_type()   :: string().
-type headers()        :: [header()].
-type header()         :: {string(), string()}.
