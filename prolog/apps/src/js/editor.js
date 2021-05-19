var myCodeMirror = CodeMirror(codemirror, {
	theme: "prolog",
	mode: "prolog",
	lineNumbers: true,
	buttons: [
		{
            hotkey: 'Ctrl-Z',
            class: '',
            label: 'Назад',
            callback: function (cm) {
                cm.execCommand('undo');
            }
        },
		{
            hotkey: 'Ctrl-Y',
            class: '',
            label: 'Вперед',
            callback: function (cm) {
                cm.execCommand('redo');
            }
        },
        {
            hotkey: '',
            class: '',
            label: 'Сохранить',
            callback: function (cm) {
                prepareFile();
            }
        },
		{
            hotkey: '',
            class: '',
            label: 'Очистить всё',
            callback: function (cm) {
                cm.setValue('');
            }
        },
    ],
});

function updateFileMenu() {
	$.ajax({
	  type: 'POST',
	  url: '/user/get-files',
	  success: function(data) {
		var dirPath = data[0];
		var fileNames = data[1];
		fileNames.splice(fileNames.length - 2, fileNames.length);
		$('#files-menu li').remove();
		fileNames.forEach(function(item, i, arr) {
			$('#files-menu').append('<li><a href="/' + dirPath + item + '">' + item + '</a></li>');
		});
	  }
	});
}

function createOrUpdateFile(fileName) {
	$.ajax({
	  type: 'POST',
	  dataType: 'json',
	  url: '/user/create-file',
	  data: {text: myCodeMirror.getValue(), name: fileName},
	  success: function(data) {
	    if (typeof data[0] != "undefined" && data[0] == 'true') {
			$('.enter-filename').modal('hide');
			$('#file-info').text('Файл: ' + data[2]);
			$('#file-info').attr('meta-path', '/' + data[1]);
			alert('Файл успешно сохранен!');
			updateFileMenu();
		}
	  }
	});
}

function setProgram(src) {
	myCodeMirror.setValue(src);
}

function loadSrc(url) {
    $.get(url)
    .done(function(program) {
		setProgram(program);
	})
	.fail(function() {
		alert('Error: ' + url + ' does not exist.');
	})
}

function prepareFile() {
	if (myCodeMirror.getValue() == '') {
		alert('Документ пуст!');
		return false;
	}
	if (confirm('Вы уверены, что хотите сохранить файл?')) {
		if ($('#file-info').text() == '') {
			$('.enter-filename').modal();
			return false;
		} else {
			var name = $('#file-info').text();
			createOrUpdateFile(name.replace('Файл: ', ''));
		}
	}
}

$(window).on("load", function() {
	updateFileMenu();
}); 

$('#confirm-filename').on('click', function() {
	if ($('#input-filename').val() == '') {
		alert('Введите название файла!');
		return false;
	}
	
	createOrUpdateFile($('#input-filename').val());
});

$("#example-menu").on("click", "a", function(evt) {
	evt.preventDefault();
	myCodeMirror.setValue('');
	window.location.hash = "";
	loadSrc(evt.target.href);
});

$('#save-file').on('click', function() {
	$.ajax({
	  type: 'POST',
	  url: '/user/create-file',
	  data: 'text=' + myCodeMirror.getValue(),
	  success: function(data) {
	    console.log(data);
	  }
	});
});

$('#files-menu').on('click', 'a', function(evt) {
	evt.preventDefault();
	myCodeMirror.setValue('');
	window.location.hash = "";
	$('#file-info').text('Файл: ' + evt.target.text);
	$('#file-info').attr('meta-path', evt.target.href);
	loadSrc(evt.target.href);
});









