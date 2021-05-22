import $ from "jquery";
import Viz from 'viz.js';
//import { SVG } from '@svgdotjs/svg.js';

class Pengine {
  hiddenNodes = {}; //Содержит скрытые узлы
  hiddenEdges = {}; //Содержит скрытые дуги
  hiddenNodesArray = []; //Массив значений узлов для наиболее быстрого отсеивания дуг
  currentNode; //Текущий узел
  currentEdge; //Текущая дуга
  canBeHiddenEdges = {}; //Содержит дуги, которые можно будет скрывать
  svgImage;
  originGraph = [];

  render(obj) {
    $('#result').show();
    var answer = obj;
    if (typeof answer === "string") {
      $('#result').html(answer);
    } else {
      let viz = new Viz();
      return viz.renderSVGElement(answer.args[0])
        .then((element) => {
          $('#result').find('svg').remove();
          $('#result').find('.alert').remove();
          $('#result').find('button').removeAttr('disabled');
          $('#result').append(element);

          //var rect = SVG('#result svg');
          //console.log(rect.find('#node1'));

          this.originGraph = this.collectGraph();

          this.svgImage = $('#result svg');
          const svgContainer = document.getElementById("result");
          let imageWidth = this.svgImage.attr('width');
          let imageHeight = this.svgImage.attr('height');

          this.svgImage.css({
            "width": "100%",
            "min-height": "600px",
            //"max-height": "100vh",
            "margin": "20px 0 20px",
            "border": "2px solid #ccc"
          });

          var viewBox = { x: 0, y: 0, w: imageWidth.substr(0, imageWidth.length - 2), h: imageHeight.substr(0, imageHeight.length - 2) };
          this.svgImage.attr("viewBox", `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
          const svgSize = { w: this.svgImage.innerWidth(), h: this.svgImage.innerHeight() };
          var isPanning = false;
          var startPoint = { x: 0, y: 0 };
          var endPoint = { x: 0, y: 0 };
          var scale = 1;

          svgContainer.onmousewheel = (e) => {
            e.preventDefault();
            var w = viewBox.w;
            var h = viewBox.h;
            var mx = e.offsetX;//mouse x  
            var my = e.offsetY;
            var dw = w * Math.sign((-1) * e.deltaY) * 0.05;
            var dh = h * Math.sign((-1) * e.deltaY) * 0.05;
            var dx = dw * mx / svgSize.w;
            var dy = dh * my / svgSize.h;
            viewBox = { x: viewBox.x + dx, y: viewBox.y + dy, w: viewBox.w - dw, h: viewBox.h - dh };
            scale = svgSize.w / viewBox.w;
            //zoomValue.innerText = `${Math.round(scale*100)/100}`;
            this.svgImage.attr('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
          }


          svgContainer.onmousedown = function (e) {
            isPanning = true;
            startPoint = { x: e.x, y: e.y };
          }

          svgContainer.onmousemove = (e) => {
            if (isPanning) {
              endPoint = { x: e.x, y: e.y };
              var dx = (startPoint.x - endPoint.x) / scale;
              var dy = (startPoint.y - endPoint.y) / scale;
              var movedViewBox = { x: viewBox.x + dx, y: viewBox.y + dy, w: viewBox.w, h: viewBox.h };
              this.svgImage.attr('viewBox', `${movedViewBox.x} ${movedViewBox.y} ${movedViewBox.w} ${movedViewBox.h}`);
            }
          }

          svgContainer.onmouseup = (e) => {
            if (isPanning) {
              endPoint = { x: e.x, y: e.y };
              var dx = (startPoint.x - endPoint.x) / scale;
              var dy = (startPoint.y - endPoint.y) / scale;
              viewBox = { x: viewBox.x + dx, y: viewBox.y + dy, w: viewBox.w, h: viewBox.h };
              this.svgImage.attr('viewBox', `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);
              isPanning = false;
            }
          }

          svgContainer.onmouseleave = function () {
            isPanning = false;
          }

          // Убираем подсказки-комментарии, которые генерируются автоматически viz.js
          $('svg').html($('svg').html().replace(/<!-[\S\s]*?-->/gm, ''));
        })
        .catch(error => {
          // Create a new Viz instance (@see Caveats page for more info)
          viz = new Viz();

          // Possibly display the error
          console.error(error);
        });
    }
  }

  //Заменяет узлы рандомным образом
  randomNodes() {
    var indexes = this.getHiddenNodeIndexes($(this.svgImage).find('.node').length);
    this.hiddenNodesArray.length = 0;
    for (var key in this.hiddenNodes) {
      delete this.hiddenNodes[key];
    }
    indexes.forEach((item) => {
      var node = '#node' + item;
      this.hiddenNodes[$(node).attr('id')] = $(node).find('text').text();
      this.hiddenNodesArray.push($(node).find('text').text());
      $(node).addClass('incomplete');
      $(node).find('text').text('?').addClass('node-text');
      $(node).find('title').text('Выберите правильный ответ, нажав на элемент');
    });
    this.setNodeMenu();
  }

  // Выбираем количество узлов и индексы узлом случайным образом
  // Возвращает массив с индексами скрываемых узлов
  getHiddenNodeIndexes(nodeCount) {
    var indexes = [];
    var count = this.randomInteger(2, nodeCount);
    while (indexes.length != count) {
      var index = this.randomInteger(1, nodeCount);
      if (indexes.indexOf(index) == -1) {
        indexes.push(index);
      }
    }
    return indexes;
  }

  randomInteger(min, max) {
    // получить случайное число от (min-0.5) до (max+0.5)
    let rand = min - 0.5 + Math.random() * (max - min + 1);
    return Math.round(rand);
  }

  // Устанавливает значения для меню выбора правильного ответа узла
  setNodeMenu() {
    $('.node-menu').html('');
    $('.node-menu').append('<h2>Возможные варианты</h2>');
    $('.node-menu').append('<ul></ul>');

    let showedNodes = Object.assign([], this.hiddenNodesArray);
    $('.node text').each((index, el) => {
      if (showedNodes.includes($(el).text())) {
        showedNodes.splice(showedNodes.indexOf($(el).text()), 1);
      }
    });

    for (let key in showedNodes) {
      $('.node-menu ul').append('<li>' + showedNodes[key] + '</li>');
    }
  }

  //Выбирает дуги, которые можно скрывать
  randomEdges() {
    for (key in this.canBeHiddenEdges) {
      delete this.canBeHiddenEdges[key];
    }
    for (key in this.hiddenEdges) {
      delete this.hiddenEdges[key];
    }
    $('svg').find('.edge').each((index, el) => {
      var title = $(el).find('title').text();
      var nodes = title.split('->');
      if (!(this.hiddenNodesArray.indexOf(nodes[0]) != -1 && this.hiddenNodesArray.indexOf(nodes[1]) != -1)) {
        this.canBeHiddenEdges[$(el).attr('id')] = { title: title, text: $(el).find('text').text() };
      }
    });
    for (var key in this.canBeHiddenEdges) {
      if (Math.round(Math.random())) {
        var edge = '#' + key;
        this.hiddenEdges[$(edge).attr('id')] = $(edge).find('text').text();
        $(edge).addClass('incomplete');
        $(edge).find('text').text('?').addClass('edge-text');
        $(edge).find('title').text('Выберите правильный ответ, нажав на элемент');
      }
    }
    this.setEdgeMenu();
  }

  // Устанавливает значения для меню выбора правильного ответа дуги
  setEdgeMenu() {
    $('.edge-menu').html('');
    $('.edge-menu').append('<h2>Возможные варианты</h2>');
    $('.edge-menu').append('<ul></ul>');
    let checkOptions = [];
    for (let key in this.hiddenEdges) {
      if (!checkOptions.includes(this.hiddenEdges[key])) {
        checkOptions.push(this.hiddenEdges[key]);
        $('.edge-menu ul').append('<li>' + this.hiddenEdges[key] + '</li>');
      }
    }
  }

  executeBinds() {
    $('body').on('click', '.node-text', (e) => {
      $('.edge-menu').hide();
      $('.node-menu').hide();
      $('.node-menu').toggle();
      this.setNodeMenu();
      if ($(e.target).text() !== "?") {
        $('.node-menu').append("<div class='remove-node'>Очистить значение</div>");
      }
      this.currentNode = $(e.target).closest('g');
    });

    $('body').on('click', '.remove-node', () => {
      $(this.currentNode).addClass('incomplete');
      $(this.currentNode).find('text').text('?').addClass('node-text');
      $(this.currentNode).find('title').text('Выберите правильный ответ, нажав на элемент');
      $(this.currentNode).find('text').removeAttr('style');
      $(this.currentNode).find('ellipse').attr('fill', 'white');
    });

    $('body').on('click', '.edge-text', (e) => {
      $('.node-menu').hide();
      $('.edge-menu').hide();
      $('.edge-menu').toggle();
      this.currentEdge = $(e.target).closest('g');
    });

    $('body').on('click', function (event) {
      var target = event.target;
      if (!(target.tagName == 'text' || target.tagName == 'LI')) {
        $('.node-menu').hide();
        $('.edge-menu').hide();
      }
    });

    $('body').on('click', '.node-menu li', (e) => {
      if (typeof this.currentNode == 'undefined') {
        return;
      }

      var textBlock = this.currentNode.children('text');
      this.currentNode.removeClass('incomplete');
      this.currentNode.children('title').text($(e.target).text());
      textBlock.text($(e.target).text());
      textBlock.css('font-size', '14px');
      textBlock.attr('fill', 'black');
      textBlock.siblings('ellipse').attr('fill', '#ffe32e');
      $('.node-menu').hide();
      $(e.target).remove();
    });

    $('body').on('click', '.edge-menu li', (e) => {
      if (typeof this.currentEdge == 'undefined') {
        return;
      }
      var textBlock = this.currentEdge.children('text');
      this.currentEdge.removeClass('incomplete');
      this.currentEdge.children('title').text($(e.target).text());
      textBlock.text($(e.target).text());
      textBlock.css('font-size', '14px');
      textBlock.attr('fill', 'black');
      textBlock.siblings('path').attr('stroke', '#a69b00');
      textBlock.siblings('polygon').attr('fill', '#a69b00');
      $('.edge-menu').hide();
    });
  }

  checkAnswers() {
    if ($('svg').find('.incomplete').length > 0) {
      if ($('#result').find('.alert').length > 0) {
        $('#result').find('.alert').text('Заполните все пропуски!');
      } else {
        $('#result').prepend('<div class="alert alert-danger">Заполните все пропуски!</div>');
      }
      return false;
    }
    $('#result').find('.alert').remove();
    var countSuccess = 0;

    let nodes = this.checkJson();
    let relations = Object.values(this.hiddenEdges);

    for (let key in nodes) {
      delete nodes[key].checked;

      if (this.hiddenNodes[nodes[key].node.id] != undefined && nodes[key].relation.id === null) {
        let node = this.originGraph.find((el) => {
          return el.relation.text == nodes[key].relation.text && el.node.text == nodes[key].node.text;
        });
        if (node !== undefined) {
          $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#49D427');
          nodes[key].correctNode = true;
          countSuccess++;
        } else {
          $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#D42727');
          nodes[key].correctNode = false;
        }

        continue;
      }

      if (this.hiddenEdges[nodes[key].relation.id] != undefined) {
        if (relations.includes(nodes[key].relation.text)) {
          let node = this.originGraph.find((el) => {
            return el.relation.text == nodes[key].relation.text && el.node.text == nodes[key].node.text;
          });
          if (node !== undefined) {
            relations.splice(relations.indexOf(nodes[key].relation.text), 1);
            this.colorEdge(nodes[key].relation.id, '#49D427');
            nodes[key].correctEdge = true;
            countSuccess++;
            if (this.hiddenNodes[nodes[key].node.id] != undefined) {
              $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#49D427');
              nodes[key].correctNode = true;
            }
          } else {
            this.colorEdge(nodes[key].relation.id, '#D42727');
            nodes[key].correctEdge = false;
            if (this.hiddenNodes[nodes[key].node.id] != undefined) {
              $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#D42727');
              nodes[key].correctNode = false;
            }
          }
        } else {
          this.colorEdge(nodes[key].relation.id, '#D42727');
          nodes[key].correctEdge = false;
          if (this.hiddenNodes[nodes[key].node.id] != undefined) {
            $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#D42727');
            nodes[key].correctNode = false;
          }
        }
      } else {
        if (this.hiddenNodes[nodes[key].node.id] != undefined) {
          let node = this.originGraph.find((el) => {
            return el.relation.text == nodes[key].relation.text && el.node.text == nodes[key].node.text;
          });
          if (node !== undefined) {
            $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#49D427');
            countSuccess++;
            nodes[key].correctNode = true;
          } else {
            $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#D42727');
            nodes[key].correctNode = false;
          }
        }
      }
    }

    // for (let key in nodes) {
    //   if (this.hiddenNodes[nodes[key].node.id] != undefined) {
    //     if (nodes[key].checked) {
    //       $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#49D427');
    //       nodes[key].correctNode = true;
    //       countSuccess++;
    //     } else {
    //       $('#' + nodes[key].node.id).find('ellipse').attr('fill', '#D42727');
    //       nodes[key].node.text = this.hiddenNodes[nodes[key].node.id];
    //       nodes[key].correctNode = false;
    //     }
    //   }

    //   delete nodes[key].checked;
    // }

    // for (let key in nodes) {
    //   if (this.hiddenEdges[nodes[key].relation.id] != undefined) {
    //     if (nodes[key].checked) {
    //       $('#' + nodes[key].relation.id).find('path').attr('stroke', '#49D427');
    //       $('#' + nodes[key].relation.id).find('polygon').attr('fill', '#49D427');
    //       nodes[key].correctEdge = true;
    //       countSuccess++;
    //     } else {
    //       if (this.hiddenEdges[nodes[key].relation.id] == $('#' + nodes[key].relation.id).find('text').text()) {
    //         $('#' + nodes[key].relation.id).find('path').attr('stroke', '#49D427');
    //         $('#' + nodes[key].relation.id).find('polygon').attr('fill', '#49D427');
    //         nodes[key].correctEdge = true;
    //         countSuccess++;
    //       } else {
    //         $('#' + nodes[key].relation.id).find('path').attr('stroke', '#D42727');
    //         $('#' + nodes[key].relation.id).find('polygon').attr('fill', '#D42727');
    //         nodes[key].correctEdge = false;
    //       }
    //     }
    //   }
    // }

    console.log(nodes);

    var result = ((countSuccess * 100) / (Object.keys(this.hiddenNodes).length + Object.keys(this.hiddenEdges).length)).toFixed(2);
    $('#result').prepend(`<div class="alert alert-success">Ваша оценка: ${result}%</div>`);
    $('#check-result-button').attr('disabled', 'disabled');
    $('.node-text').removeClass('node-text');
    $('.edge-text').removeClass('edge-text');

    return nodes;
  }

  colorEdge(id, color) {
    $('#' + id).find('path').attr('stroke', color);
    $('#' + id).find('polygon').attr('fill', color);
  }

  collectGraph() {
    let graph = [];
    let count = $('.node').length;
    let arrayCount = 1;
    graph[0] = {
      relation: {
        id: null,
        text: null,
      },
      node: {
        id: $('#node1').attr('id'),
        text: $('#node1').children('text').text(),
      }
    };
    for (let i = 2; i <= count; i++) {
      graph[arrayCount] = {
        relation: {
          id: $('#edge' + (i - 1)).attr('id'),
          text: $('#edge' + (i - 1)).children('text').text(),
        },
        node: {
          id: $('#node' + i).attr('id'),
          text: $('#node' + i).children('text').text(),
        }
      };
      arrayCount++;
    }

    return graph;
  }

  checkJson() {
    let finishedGraph = this.collectGraph();

    let originG = [];
    for (let item in this.originGraph) {
      originG.push(this.getProp(this.originGraph[item]));
    }

    for (let item in finishedGraph) {
      if (finishedGraph[item].relation.id === null) {
        if (!originG.some(code => (code.node.text === finishedGraph[item].node.text && code.relation.id === null))) {
          finishedGraph[item].checked = false;
        } else {
          finishedGraph[item].checked = true;
        }
      } else {
        if (!originG.some(code => (code.relation.text === finishedGraph[item].relation.text && code.node.text === finishedGraph[item].node.text))) {
          finishedGraph[item].checked = false;
        } else {
          finishedGraph[item].checked = true;
        }
      }
    }

    return finishedGraph;
  }

  getProp(o) {
    if (typeof (o) === 'object') {
      o = Object.assign({}, o);
    }

    for (var prop in o) {
      if (typeof (o[prop]) === 'object') {
        o[prop] = Object.assign({}, o[prop]);
        this.getProp(o[prop]);
      }
    }

    return o;
  }
}

export default Pengine;