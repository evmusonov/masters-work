:- module(functions,
	[
		get_uid/2	
	]).

get_uid(Input, Output) :-
	get_time(Time),
	number_string(Time, TimeString),
	atom_string(Input, InputString),
	string_concat(InputString, TimeString, RandomInput),
	sha_hash(RandomInput, Hash, [algorithm(sha256)]),
	hash_atom(Hash, Hashex),
	string_to_atom(Output, Hashex).

get_session_message(Key, Message) :-
	( http_session_data(Message, Key)
	-> http_session_retract(Message, Key)
	;  Message = ''
	).





	
	                 
                    