<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <div id="graph"></div>
    </div>
  </div>
</template>

<script>
import Viz from "viz.js";

export default {
  name: "Home",
  props: {
    msg: String,
  },
  data() {
    return {
      info: null,
    };
  },
  mounted() {
    let src = `
              digraph G {

                subgraph cluster_0 {
                  style=filled;
                  color=lightgrey;
                  node [style=filled,color=white];
                  a0 -> a1 -> a2 -> a3;
                  label = "process #1";
                }

                subgraph cluster_1 {
                  node [style=filled];
                  b0 -> b1 -> b2 -> b3;
                  label = "process #2";
                  color=blue
                }
                start -> a0;
                start -> b0;
                a1 -> b3;
                b2 -> a3;
                a3 -> a0;
                a3 -> end;
                b3 -> end;

                start [shape=Mdiamond];
                end [shape=Msquare];
              }
            `;
    let viz = new Viz();
    viz
      .renderSVGElement(src)
      .then(function (element) {
        document.querySelector("#graph").append(element);
      })
      .catch((error) => {
        // Create a new Viz instance (@see Caveats page for more info)
        viz = new Viz();

        // Possibly display the error
        console.error(error);
      });

    // this.$loadScript("/node_modules/viz.js/viz.js")
    //   .then(() => {
    //     // Script is loaded, do something
    //   })
    //   .catch(() => {
    //     // Failed to fetch script
    //   });
    // this.$loadScript("/node_modules/viz.js/viz.js")
    //   .then(() => {
    //     this.$loadScript("/node_modules/viz.js/full.render.js")
    //       .then(() => {
    //         let src = `
    //           digraph G {

    //             subgraph cluster_0 {
    //               style=filled;
    //               color=lightgrey;
    //               node [style=filled,color=white];
    //               a0 -> a1 -> a2 -> a3;
    //               label = "process #1";
    //             }

    //             subgraph cluster_1 {
    //               node [style=filled];
    //               b0 -> b1 -> b2 -> b3;
    //               label = "process #2";
    //               color=blue
    //             }
    //             start -> a0;
    //             start -> b0;
    //             a1 -> b3;
    //             b2 -> a3;
    //             a3 -> a0;
    //             a3 -> end;
    //             b3 -> end;

    //             start [shape=Mdiamond];
    //             end [shape=Msquare];
    //           }
    //         `;
    //         // eslint-disable-next-line
    //         let viz = new Viz();
    //         viz
    //           .renderSVGElement(src)
    //           .then(function (element) {
    //             document.querySelector("#graph").append(element);
    //           })
    //           .catch((error) => {
    //             // Create a new Viz instance (@see Caveats page for more info)
    //             // eslint-disable-next-line
    //             viz = new Viz();

    //             // Possibly display the error
    //             console.error(error);
    //           });
    //       })
    //       .catch(() => {
    //         // Failed to fetch script
    //       });
    //   })
    //   .catch(() => {
    //     // Failed to fetch script
    //   });
  },
};
</script>
