-include("gen_types.hrl").
-include("json_types.hrl").

-type bridge_command() :: 'JOINWORKERPOOL'
			| 'JOINCHANNEL'
			| 'LEAVEWORKERPOOL'
			| 'LEAVECHANNEL'
			| 'GETCHANNEL'
			| 'CONNECT'
			| 'SEND'.


-type remote_service() :: {proplist(ref, [json_key()])}.

-type service() :: function()
		 | pid()
		 | remote_service()
		 | undefined.

-type options() :: proplist(atom(), any()).
