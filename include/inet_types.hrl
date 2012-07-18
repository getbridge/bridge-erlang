-include("gen_types.hrl").

-type address() :: ip_address() | hostname().
-type hostname() :: atom() | str().
-type ip_address() :: ip4_address() | ip6_address().
-type ip4_address() :: { 0..255, 0..255, 0..255, 0..255 }.

-type ip6_address() :: { 0..65535, 0..65535, 0..65535, 0..65535, 
			 0..65535, 0..65535, 0..65535, 0..65535  }.

-type httpc_result() :: {status_line(), headers(), httpc_body()}
		      | {status_code(), httpc_body()}.

-type httpc_body() :: str() | binary().


-type status_line()    :: {http_version(), status_code(), reason_phrase()}.
-type http_version()   :: str().
-type status_code()    :: pos_integer().
-type reason_phrase()  :: str().
-type content_type()   :: str().
-type headers()        :: [header()].
-type header()         :: {field(), value()}.
-type field()          :: str().
-type value()          :: str().
