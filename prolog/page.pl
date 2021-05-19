:- module(page,
          [

          ]).

:- use_module(library(http/html_write)).

:- multifile
        user:body//2,
		user:head//2.

user:body(main, Body) -->
	html(body([
		\header,
		div(class('content'), div(class(container), Body)),
		\footer
	])).

user:body(admin, Body) -->
	html(body([
		\header_admin,
		div(class('content'), div(class(container), Body)),
		\footer
	])).

user:head(main, Head) -->
	html(head([ 
		link([rel('stylesheet'), href('/apps/src/vendor/bootstrap/css/bootstrap.min.css')]),
		link([rel('stylesheet'), href('/apps/node_modules/codemirror/lib/codemirror.css')]),
		link([rel('stylesheet'), href('/apps/node_modules/codemirror/theme/prolog.css')]),
		link([rel('stylesheet'), href('/apps/vendor/codemirror-buttons/buttons.css')]),
		%link([rel('stylesheet'), href('/apps/src/css/editor.css')]),
		link([rel('stylesheet'), href('/apps/src/css/style.css')]),
		script([src('/apps/src/js/jquery.js'),type('text/javascript'), charset('utf-8')], []),
		Head
	])).

user:head(admin, Head) -->
	html(head([ 
		link([rel('stylesheet'), href('/apps/src/vendor/bootstrap/css/bootstrap.min.css')]),
		link([rel('stylesheet'), href('/apps/src/css/style.css')]),
		script([src('/apps/src/js/jquery.js'),type('text/javascript'), charset('utf-8')], []),
		Head
	])).

% head(Scripts, Styles) -->
% 	{Basic = [
% 		title('Editor 2.0')
% 	],
% 	BasicStyles = [
% 		link([rel="stylesheet",href="/apps/src/vendor/bootstrap/css_3.2/bootstrap.min.css"], []),
% 		link([rel="stylesheet",href="/apps/node_modules/codemirror/lib/codemirror.css"], []),
% 		link([rel="stylesheet",href="/apps/node_modules/codemirror/theme/prolog.css"], []),
% 		link([rel="stylesheet",href="/apps/vendor/codemirror-buttons/buttons.css"], []),
% 		link([rel="stylesheet",href="/apps/src/css/editor.css"], [])
% 	],
% 	BasicScripts = [
% 		script([src="/apps/src/js/jquery.js",type="text/javascript",charset="utf-8"], [])
% 	],
% 	append(Basic, BasicStyles, BasicTemp),
% 	append(BasicTemp, Styles, BasicTemp1),
% 	append(BasicTemp1, BasicScripts, BasicTemp2),
% 	append(BasicTemp2, Scripts, BasicTemp3)},
% 	html(BasicTemp3).

header -->
	html(
		header(class='navbar header',
			div(class='container', [
				div(class='navbar-header', [
					a(
						[href='/',style='margin-left:30px;font-size:20px',class='navbar-brand'],
						'IntelAgent'
					)]
				),
				ul(class='nav nav-pills', [
					li(class='nav-item dropdown', [
						a([href="#",class="nav-link dropdown-toggle",data-toggle="dropdown",role="button",aria-haspopup="true",aria-expanded="false"],
							[
								'Примеры',
								b(class=caret, [])
							]
						),
						div([class="dropdown-menu",class="dropdown-menu"],
							a([href="/apps/scratchpad/examples/parents_tree.pl",class="dropdown-item"],
								'Parents Tree'
							)
						)]
					),
					li(class='nav-item',
						a([href="/user/editor",class="nav-link"], 'Редактор')
					),
					li(class='nav-item',
						a([href="/admin",class="nav-link"], 'Панель управления')
					),
					li(class='nav-item',
						a([href="/user/logout",class="nav-link"], 'Выйти')
					)]
				)]
			)
		)
	).

header_admin -->
	html(
		header(class='navbar header',
			div(class='container', [
				div(class='navbar-header', [
					a(
						[href='/',style='margin-left:30px;font-size:20px',class='navbar-brand'],
						'IntelAgent'
					)]
				),
				ul(class='nav nav-pills', [
					li(class='nav-item',
						a([href="/admin/courses",class="nav-link"], 'Курсы')
					),
					li(class='nav-item',
						a([href="/user/logout",class="nav-link"], 'Выйти')
					)]
				)]
			)
		)
	).

footer -->
    html([
		div([class('footer')],
			div(class(container), [div(class(row), div(class('col-sm-12'), 'footer'))])
		),
		script(src="/apps/node_modules/viz.js/viz.js", []),
		script(src="/apps/node_modules/viz.js/full.render.js", []),
		script(src="/apps/src/vendor/bootstrap/js/bootstrap.min.js", []),
		script(src="/apps/src/js/pengines.js", []),
		script(src="/apps/node_modules/codemirror/lib/codemirror.js", []),
		script(src="/apps/node_modules/codemirror/mode/prolog/prolog.js", []),
		script(src="/apps/node_modules/codemirror/mode/prolog/prolog_server.js", []),
		script(src="/apps/src/js/main.js", []),
		script(src="https://togetherjs.com/togetherjs-min.js", [])
	]).
	
	
	