:- module(db_users,
          [ attach_db/1,           % +File
            current_user_data/2,        % ?User, ?Role
            add_user/4,                 % +User, +Role
            set_user_data/2,            % +User, +Role
	    user_data/4
          ]).
:- use_module(library(persistency)).

:- persistent
        user_data(uid:string, email:string, password:string, role:integer).

attach_db(File) :-
        db_attach(File, []).

%%      current_user_role(+Name, -Role) is semidet.

current_user_data(Name, Role) :-
        with_mutex(data_db, user_data(Name, Role)).

add_user(Uid, Email, Password, Role) :-
        assert_user_data(Uid, Email, Password, Role).

set_user_data(Name, Role) :-
        user_data(Name, Role), !.
set_user_data(Name, Role) :-
        with_mutex(data_db,
                   (  retractall_user_data(Name, _),
                      assert_user_data(Name, Role))).