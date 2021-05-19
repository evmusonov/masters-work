:- module(admin,
          [

          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(dcg/high_order)).
:- use_module(page).
:- use_module(db_courses).
:- use_module(user_module).

http:location(admin, root(admin), []).

:- http_handler(admin(.), main_page, []).
:- http_handler(admin(courses), courses_page, []).
:- http_handler(admin(courses/add), courses_add_page, []).
:- http_handler(admin(courses/create), courses_create, []).

:- initialization db_courses:attach_db('db/courses').

% Параметры для форм
param(course_name, [length >= 5]).

main_page(Request) :-
	user_module:check_session(Request),
	reply_html_page(admin, [title('Панель управления')], \main_page_body).

main_page_body -->
	foreach(user_data(_, Email, _, _), html(li(Email))).

courses_page(Request) :-
	user_module:check_session(Request),
	reply_html_page(admin, [title('Курсы')], \courses_body).

courses_body -->
	{
		functions:get_session_message(create_course_message, Message),
		( \=(Message, '')
		-> MessageDiv = div(class('alert alert-success'), Message)
		;  MessageDiv = ''
		)
	},
	html([
		div(class(row),
			div(class('col-sm-12'), [
				MessageDiv,
				h1('Список курсов'),
				a(href('/admin/courses/add'), 
					button([type("button"),class("btn btn-success")], 'Добавить курс')
				),
				div(class('mt-3'), [
					\foreach(course_data(_, Name, _, _), html(
						div([class('card mb-3')],
							div(class('card-body'), [
								h5(class('card-title'), a(href('#'), Name)),
								div([class('btn-group'),role('group')], [
									a(href('#'), 'Редактировать'),
									a(href('#'), 'Удалить')
								])
							])
						)
					)) 
				])
			])
		)	
	]).

courses_add_page(Request) :-
	user_module:check_session(Request),
	reply_html_page(admin, [title('Добавление курса')], \courses_add_body).

courses_create(Request) :-
	user_module:check_session(Request),
	member(method(post), Request), !,
	http_parameters(Request,
		[
			course_name(CourseName)
		],
		[attribute_declarations(param)]
    ),
	create_course(CourseName, Message),
	http_session_asserta(Message, create_course_message),
	http_redirect(moved, admin(courses), Request).

create_course(CourseName, Message) :-
	( course_data(_, CourseName, _, _)
	-> Message = 'Данный курс уже существует'
	;  functions:get_uid(CourseName, Uid),
	   user_module:get_user_uid(UserUid),
	   db_courses:add_course(Uid, CourseName, UserUid, 0),
	   Message = 'Курс успешно добавлен'
	).

courses_add_body -->
	html([
		div(class(row),
			div(class('col-sm-12'), [
				h1('Добавление курса'),
				form([action('/admin/courses/create'),method('POST')], [
					div([class('form-group')], [
						label([], 'Название курса'),
						input([type('text'),class('form-control'),name('course_name')], [])
					]),
					button([type('submit'),class('btn btn-primary')], 'Отправить')
				])
			])
		)	
	]).
	                 
                    