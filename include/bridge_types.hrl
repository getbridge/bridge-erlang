-type json_key() :: atom()
		    | binary().

-type proplist(K, V) :: [{K, V}].

-type json_obj() :: {proplist(json_key(), json())}.

-type json() :: json_obj()
	      | [json()]
	      | json_key()
	      | number().

-type remote_service() :: {proplist(ref, [json_key()])}.

-type service() :: remote_service()
		 | function()
		 | pid()
		 | undefined.

-type bridge_command() :: 'JOINWORKERPOOL'
			| 'JOINCHANNEL'
			| 'LEAVEWORKERPOOL'
			| 'LEAVECHANNEL'
			| 'GETCHANNEL'
			| 'CONNECT'
			| 'SEND'.

-type socket() :: pid() | {sslsocket, new_ssl, pid()}.
