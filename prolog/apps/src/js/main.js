$(window).on("load", function() {
	
}); 

var viz = new Viz();

var myCodeMirror = CodeMirror(codemirror, {
	theme: "prolog",
	mode: "prolog",
	lineNumbers: true
});

var myCodeMirrorQuery = CodeMirror(codemirror_query, {
	theme: "prolog",
	mode: "prolog"
});

function setProgram(src) {
	myCodeMirror.setValue(src);
	myCodeMirrorQuery.setValue("graf(_,_,мария,G).");
	// $('#run-btn').prop('disabled', false);
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

$("#example-menu").on("click", "a", function(evt) {
	evt.preventDefault();
	myCodeMirror.setValue('');
	window.location.hash = "";
	loadSrc(evt.target.href);
});


function startPengine(src_text, ask_text) {
	var pengine = new Pengine({
		src: src_text,
		ask: ask_text,
		onsuccess: pengineSuccess
	});
}

function pengineSuccess(obj) {
	$('#result').show();
	var answer = obj.data[0][obj.projection[0]];
	console.log(answer);
	if (typeof answer === "string") {
		$('#result').html(answer);
	} else {
		viz.renderSVGElement(answer.args[0])
		.then(function(element) {
			$('#result').find('svg').remove();
			$('#result').find('.alert').remove();
			$('#result').find('button').removeAttr('disabled');
	    	$('#result').append(element);




		
		
		
		const svgImage = $('#result svg');
		const svgContainer = document.getElementById("result");
		
		randomNodes(svgImage);
		randomEdges();
		
		svgImage.css({
			"width": "100%",
			"max-height": "400px",
			"margin": "20px 0 20px",
			"border": "2px solid #ccc"
		});
		
		var viewBox = {x:0,y:0,w:svgImage.innerWidth(),h:svgImage.innerHeight()};
		svgImage.attr("viewBox", `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
		const svgSize = {w:svgImage.innerWidth(),h:svgImage.innerHeight()};
		var isPanning = false;
		var startPoint = {x:0,y:0};
		var endPoint = {x:0,y:0};
		var scale = 1;
		
		svgContainer.onmousewheel = function(e) {
		   e.preventDefault();
		   var w = viewBox.w;
		   var h = viewBox.h;
		   var mx = e.offsetX;//mouse x  
		   var my = e.offsetY;    
		   var dw = w*Math.sign(e.deltaY)*0.05;
		   var dh = h*Math.sign(e.deltaY)*0.05;
		   var dx = dw*mx/svgSize.w;
		   var dy = dh*my/svgSize.h;
		   viewBox = {x:viewBox.x+dx,y:viewBox.y+dy,w:viewBox.w-dw,h:viewBox.h-dh};
		   scale = svgSize.w/viewBox.w;
		   //zoomValue.innerText = `${Math.round(scale*100)/100}`;
		   svgImage.attr('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
		}
		
		
		svgContainer.onmousedown = function(e){
		   isPanning = true;
		   startPoint = {x:e.x,y:e.y};   
		}
		
		svgContainer.onmousemove = function(e){
		   if (isPanning){
		  endPoint = {x:e.x,y:e.y};
		  var dx = (startPoint.x - endPoint.x)/scale;
		  var dy = (startPoint.y - endPoint.y)/scale;
		  var movedViewBox = {x:viewBox.x+dx,y:viewBox.y+dy,w:viewBox.w,h:viewBox.h};
		  svgImage.attr('viewBox', `${movedViewBox.x} ${movedViewBox.y} ${movedViewBox.w} ${movedViewBox.h}`);
		   }
		}
		
		svgContainer.onmouseup = function(e){
		   if (isPanning){ 
		  endPoint = {x:e.x,y:e.y};
		  var dx = (startPoint.x - endPoint.x)/scale;
		  var dy = (startPoint.y - endPoint.y)/scale;
		  viewBox = {x:viewBox.x+dx,y:viewBox.y+dy,w:viewBox.w,h:viewBox.h};
		  svgImage.attr('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
		  isPanning = false;
		   }
		}
		
		svgContainer.onmouseleave = function(e){
		 isPanning = false;
		}
	  })
	  .catch(error => {
	    // Create a new Viz instance (@see Caveats page for more info)
	    viz = new Viz();
	
	    // Possibly display the error
	    console.error(error);
	  });
	}
}

$("#run-btn").on("click", function() {
	startPengine(myCodeMirror.getValue(), myCodeMirrorQuery.getValue());
});

var hiddenNodes = {}; //Содержит скрытые узлы
var hiddenEdges = {}; //Содержит скрытые дуги
var hiddenNodesArray = []; //Массив значений узлов для наиболее быстрого отсеивания дуг
var currentNode; //Текущий узел
var currentEdge; //Текущая дуга

var canBeHiddenEdges = {}; //Содержит дуги, которые можно будет скрывать

//Заменяет узлы рандомным образом
function randomNodes(svgImage) {
	var indexes = getHiddenNodeIndexes($(svgImage).find('.node').length);
	hiddenNodesArray.length = 0;
	for (var key in hiddenNodes) {
    	delete hiddenNodes[key];
	}
	indexes.forEach(function(item, index, array) {
		var node = '#node' + item;
		hiddenNodes[$(node).attr('id')] = $(node).find('text').text();
		hiddenNodesArray.push($(node).find('text').text());
		$(node).addClass('incomplete');
		$(node).find('text').text('?').addClass('node-text');
		$(node).find('title').text('Выберите правильный ответ, нажав на элемент');
	});
	setNodeMenu();
}

// Выбираем количество узлов и индексы узлом случайным образом
// Возвращает массив с индексами скрываемых узлов
function getHiddenNodeIndexes(nodeCount) {
	var indexes = [];
	var count = randomInteger(2, nodeCount);
	while (indexes.length != count) {
		var index = randomInteger(1, nodeCount);
		if (indexes.indexOf(index) == -1) {
			indexes.push(index);
		}
	}
	return indexes;
}

function randomInteger(min, max) {
  // получить случайное число от (min-0.5) до (max+0.5)
  let rand = min - 0.5 + Math.random() * (max - min + 1);
  return Math.round(rand);
}

// Устанавливает значения для меню выбора правильного ответа узла
function setNodeMenu() {
	$('.node-menu').html('');
	$('.node-menu').append('<h2>Возможные варанты</h2>');
	$('.node-menu').append('<ul></ul>');
	for(key in hiddenNodes) {
		$('.node-menu ul').append('<li>' + hiddenNodes[key] + '</li>');
	}
}

//Выбирает дуги, которые можно скрывать
function randomEdges() {
	for (var key in canBeHiddenEdges) {
    	delete canBeHiddenEdges[key];
	}
	for (var key in hiddenEdges) {
    	delete hiddenEdges[key];
	}
	$('svg').find('.edge').each(function(edge) {
		var title = $(this).find('title').text();
		var nodes = title.split('->');
		if (!(hiddenNodesArray.indexOf(nodes[0]) != -1 && hiddenNodesArray.indexOf(nodes[1]) != -1)) {
			canBeHiddenEdges[$(this).attr('id')] = {title: title, text: $(this).find('text').text()};
		}
	});
	console.log(canBeHiddenEdges);
	for (var key in canBeHiddenEdges) {
		if (Math.round(Math.random())) {
			var edge = '#' + key;
			hiddenEdges[$(edge).attr('id')] = $(edge).find('text').text();
			$(edge).addClass('incomplete');
			$(edge).find('text').text('?').addClass('edge-text');
			$(edge).find('title').text('Выберите правильный ответ, нажав на элемент');
		}
	}
	setEdgeMenu();
}

// Устанавливает значения для меню выбора правильного ответа дуги
function setEdgeMenu() {
	$('.edge-menu').html('');
	$('.edge-menu').append('<h2>Возможные варанты</h2>');
	$('.edge-menu').append('<ul></ul>');
	for(key in hiddenEdges) {
		$('.edge-menu ul').append('<li>' + hiddenEdges[key] + '</li>');
	}
}

$('body').on('click', '.node-text', function() {
	$('.edge-menu').hide();
	$('.node-menu').hide();
	$('.node-menu').toggle();
	currentNode = $(this).closest('g');
});

$('body').on('click', '.edge-text', function() {
	$('.node-menu').hide();
	$('.edge-menu').hide();
	$('.edge-menu').toggle();
	currentEdge = $(this).closest('g');
});

$('body').on('click', function(event) {
	var target = event.target;
	if (!(target.tagName == 'text' || target.tagName == 'LI')) {
        $('.node-menu').hide();
		$('.edge-menu').hide();
    }
});

$('body').on('click', '.node-menu li', function() {
	if (typeof currentNode == 'undefined') {
		return;
	}
	var textBlock = currentNode.children('text');
	currentNode.removeClass('incomplete');
	currentNode.children('title').text($(this).text());
	textBlock.text($(this).text());
	textBlock.css('font-size', '14px');
	textBlock.attr('fill', 'black');
	textBlock.siblings('ellipse').attr('fill', '#ffe32e');
	$('.node-menu').hide();
});

$('body').on('click', '.edge-menu li', function() {
	if (typeof currentEdge == 'undefined') {
		return;
	}
	var textBlock = currentEdge.children('text');
	currentEdge.removeClass('incomplete');
	currentEdge.children('title').text($(this).text());
	textBlock.text($(this).text());
	textBlock.css('font-size', '14px');
	textBlock.attr('fill', 'black');
	textBlock.siblings('path').attr('stroke', '#a69b00');
	textBlock.siblings('polygon').attr('fill', '#a69b00');
	$('.edge-menu').hide();
});

//Сверка значений ответов
$('#check-result-button').on('click', function() {
	if ($('svg').find('.incomplete').length > 0) {
		if ($('#result').find('.alert').length > 0) {
			$('#result').find('.alert').text('Заполните все пропуски!');
		} else {
			$('#result').prepend('<div class="alert alert-danger">Заполните все пропуски!</div>');
		}
		return;
	}
	$('#result').find('.alert').remove();
	var countSuccess = 0;
	for(key in hiddenNodes) {
		if (hiddenNodes[key] == $('#' + key).find('text').text()) {
			$('#' + key).find('ellipse').attr('fill', '#49D427');
			countSuccess++;
		} else {
			$('#' + key).find('ellipse').attr('fill', '#D42727');
		}
	}
	for(key in hiddenEdges) {
		if (hiddenEdges[key] == $('#' + key).find('text').text()) {
			$('#' + key).find('path').attr('stroke', '#49D427');
			$('#' + key).find('polygon').attr('fill', '#49D427');
			countSuccess++;
		} else {
			$('#' + key).find('path').attr('stroke', '#D42727');
			$('#' + key).find('polygon').attr('fill', '#D42727');
		}
	}
	var result = ((countSuccess * 100) / (Object.keys(hiddenNodes).length + Object.keys(hiddenEdges).length)).toFixed(2);
	$('#result').prepend(`<div class="alert alert-success">Ваша оценка: ${result}%</div>`);
	$('#check-result-button').attr('disabled', 'disabled');
	$('.node-text').removeClass('node-text');
	$('.edge-text').removeClass('edge-text');
});










