import $ from "jquery";

class GraphProgress {
  check(answers) {
      $('.node').find('ellipse').attr('fill', '#cccccc');
      $('.edge').find('path').attr('stroke', '#333333');
      $('.edge').find('polygon').attr('fill', '#333333');

    if (answers.length > 0) {
      for (let i in answers) {
        for (let j in answers[i].answer) {
          if (answers[i].answer[j].correctNode !== undefined) {
            if (answers[i].answer[j].correctNode) {
              $('.node title').filter(function () {
                return $(this).text() === answers[i].answer[j].node.text;
              }).siblings('ellipse').attr('fill', '#49D427');
            } else {
              $('.node title').filter(function () {
                return $(this).text() === answers[i].answer[j].node.text;
              }).siblings('ellipse').attr('fill', '#D42727');
            }
          }

          // if (answers[i].answer[j].correctEdge !== undefined) {
          //   if (answers[i].answer[j].correctEdge) {
          //     $(`.edge title:contains("${answers[i].sub}->${answers[i].answer[j].node.text}")`).siblings('path').attr('stroke', '#49D427');
          //     $(`.edge title:contains("${answers[i].sub}->${answers[i].answer[j].node.text}")`).siblings('polygon').attr('fill', '#49D427');
          //   } else {
          //     $(`.edge title:contains("${answers[i].sub}->${answers[i].answer[j].node.text}")`).siblings('path').attr('stroke', '#D42727');
          //     $(`.edge title:contains("${answers[i].sub}->${answers[i].answer[j].node.text}")`).siblings('polygon').attr('fill', '#D42727');
          //   }
          // }
          if (answers[i].answer[j].correctEdge !== undefined) {
            if (answers[i].answer[j].correctEdge) {
              $('.edge title').filter(function () {
                return $(this).text() === answers[i].sub + '->' + answers[i].answer[j].node.text;
              }).siblings('path').attr('stroke', '#49D427');
              $('.edge title').filter(function () {
                return $(this).text() === answers[i].sub + '->' + answers[i].answer[j].node.text;
              }).siblings('polygon').attr('fill', '#49D427');
            } else {
              $('.edge title').filter(function () {
                return $(this).text() === answers[i].sub + '->' + answers[i].answer[j].node.text;
              }).siblings('path').attr('stroke', '#D42727');
              $('.edge title').filter(function () {
                return $(this).text() === answers[i].sub + '->' + answers[i].answer[j].node.text;
              }).siblings('polygon').attr('fill', '#D42727');
            }
          }
        }
      }
    }
  }
}

export default GraphProgress;