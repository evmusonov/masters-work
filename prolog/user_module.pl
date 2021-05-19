:- module(user_module,
          [
          	register/1,
          	check_session/1,
			get_user_name/1,
			get_user_uid/1
          ]).

:- use_module(page).

http:location(user, root(user), []).

:- http_handler(root(api/user/register), register, [id(register_handler)]).
:- http_handler(user(login), login, [id(login_handler)]).
:- http_handler(user(logout), logout, [id(logout_handler)]).
:- http_handler(root(user), main_page, [id(user_handler)]).
:- http_handler(user(editor), editor, []).
:- http_handler(root(check), check_session, []).
:- http_handler(user(id), show_id, []).
:- http_handler(user('create-file'), create_file, []).
:- http_handler(user('get-files'), get_files, []).

param(email, [optional(true)]).
param(text, [optional(true)]).
param(password,  [length >= 2]).

%%%%%%%%%%%%%%
% User roles
% 0 - email is not verified
% 1 - Usual user
% 2 - Editor
% 3 - Admin
%%%%%%%%%%%%%%

show_id(_Request) :-
	http_session_id(Id),
	format('Content-type: text/html~n~n', []),
	format('~a', [Id]).

register(Request) :-
	http_read_json_dict(Request, JSONDict), !,
	member(method(post), Request), !,
	register_result(JSONDict.get(email), JSONDict.get(password), Message),
	reply_json_dict(_{status:'true', message:Message}).
	
register(_Request) :-
	reply_json_dict(_{status:'true', message:'Введите данные'}).

register_result(Email, Password, Message) :-
	( user_data(_, Email, _)
	-> Message = 'Данный e-mail уже зарегистрирован'
	;  functions:get_uid(Email, Uid),
	   data_db:add_user(Uid, Email, Password, 1),
	   Message = 'Вы успешно зарегистрированы'
	).

login(Request) :-
	member(method(post), Request), !,
	http_parameters(Request,
		[
			email(Email),
			password(Password)
		],
		[attribute_declarations(param)]
    ),
	login_result(Uid, Email, Password, Role, Request),
	http_session_id(Id),
	http_session_assert([Uid, Email, Password, Role], Id),
	http_redirect(moved, root(user), Request).

login_result(Uid, Email, Password, Role, Request) :-
	( user_data(Uid, Email, Password, Role)
	-> true
	;  http_session_asserta('Неверные данные', enter_message),
	   http_redirect(moved, root(.), Request)
	).


user(Request) :-
	check_session(Request),
	%http_reply_file('apps/src/index.html', [], Request).
	reply_html_page(main, [title('Hello')], [div('Hhahaha')]).

main_page(Request) :-
	check_session(Request),
	reply_html_page(main, \main_page_head, \main_page_body).

main_page_head -->
	html([
		title('Мои курсы')
	]).

main_page_body -->
	html([
		div(class(row),
			div(class('col-sm-12'), [
				nav(div([class('nav nav-tabs'),id('nav-tab'),role('tablist')], [
					a([
						class("nav-link active"),
						id("nav-home-tab"),
						'data-toggle'("tab"),
						href("#nav-my-courses"),
						role("tab"),
						'aria-controls'("nav-my-courses"),
						'aria-selected'("true")], 'Мои курсы'),
					a([
						class("nav-link"),
						id("nav-home-tab"),
						'data-toggle'("tab"),
						href("#nav-available"),
						role("tab"),
						'aria-controls'("nav-available"),
						'aria-selected'("false")], 'Доступные курсы')
				])),
				div([class("tab-content"),id("nav-tabContent")], [
					div([
						class("tab-pane fade show active"),
						id("nav-my-courses"),role("tabpanel"),
						'aria-labelledby'("nav-my-courses-tab")], 'My courses'),
					div([
						class("tab-pane fade"),
						id("nav-available"),role("tabpanel"),
						'aria-labelledby'("nav-available-tab")], 'Available courses')
				])
			])
		)	
	]).

editor(Request) :-
	%format('Content-type: text/html~n~n', []),
	%format('~w', [Request]).
	check_session(Request),
	%render:reply_page([], []).
	%reply_html_page(main, [title('Hello')], [div('Hhahaha')]).
	http_reply_file('apps/src/index.html', [], Request).


logout(Request) :-
	http_session_id(Id),
	http_session_data(Data, Id),
	http_session_retract(Data, Id),
	http_redirect(moved, root(.), Request).
                    
check_session(Request) :-
	http_session_id(Id),
	( memberchk(path('/'), Request)
	-> ( http_session_data(_, Id)
	   -> http_redirect(moved, root(user), Request)
	   ;  true
	   )
	;  ( http_session_data(_, Id)
	   -> true
	   ;  http_redirect(moved, root(.), Request)
	   )
	).

get_user_uid(Uid) :-
	http_session_id(Id),
	http_session_data(Data, Id),
	arg(1, Data, Uid).

get_user_name(Name) :-
	http_session_id(Id),
	http_session_data(Data, Id),
	arg(2, Data, Name).
	
get_dir_path(Uid, DirPath) :-
	atom_string(Uid, UidString),
	string_concat('apps/files/', UidString, DirPath).
	
get_file_path(DirPath, FileName, FilePath) :-
	string_concat(DirPath, '/', CorrectDirPath),
	string_concat(CorrectDirPath, FileName, FilePath).
	
create_file(Request) :-
	check_session(Request),
	get_user_uid(Uid),
	member(method(post), Request), !,
	http_parameters(Request,
                [
                  text(Text, []),
                  name(Name, [])
                ]
        ),
	

	get_dir_path(Uid, DirPath),
	get_file_path(DirPath, Name, FilePath),

	( exists_directory(DirPath)
	-> true
	;  make_directory(DirPath)
	),
	open(FilePath, write, Stream),
	write(Stream, Text),
	close(Stream),
	reply_json(['true', FilePath, Name]).
	
get_files(Request) :-
	check_session(Request),
	get_user_uid(Uid),
	get_dir_path(Uid, DirPath),
	directory_files(DirPath, Files),
	reply_json([DirPath, Files]).





	
	                 
                    