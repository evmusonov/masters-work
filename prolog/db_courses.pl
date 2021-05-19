:- module(db_courses,
          [ attach_db/1,           % +File
            add_course/4,                 % +User, +Role
	    course_data/4
          ]).
:- use_module(library(persistency)).

:- persistent
        course_data(uid:atom, name:atom, userUid:atom, vis:integer).

attach_db(File) :-
        db_attach(File, []).

add_course(Uid, Name, UserUid, Visible) :-
        assert_course_data(Uid, Name, UserUid, Visible).
