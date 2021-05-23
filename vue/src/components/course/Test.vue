<template>
  <div class="p-grid">
    <ConfirmDialog :style="{ width: '50vw' }"></ConfirmDialog>
    <div class="p-col-12 p-text-center" v-if="test">
      <h1>
        Тест по курсу <span>"{{ test.course.name }}"</span>
      </h1>
      <div>
        <ProgressSpinner
          class="p-mt-3"
          animationDuration=".6s"
          style="width: 50px; height: 50px"
          v-if="!loading"
        />
      </div>
    </div>
    <div class="p-col-12 p-text-center" v-if="test">
      <div id="result" style="display: none">
        <div class="node-menu" style="display: none"></div>
        <div class="edge-menu" style="display: none"></div>
        <div>
          <Button
            class="p-mt-3 p-button-rounded p-button-primary"
            label="Проверить ответы"
            @click="checkAnswers"
          />
        </div>
        <div>
          <Button
            class="p-mt-3 p-button-rounded p-button-success"
            label="Следующее задание"
            @click="next"
            v-if="isNext"
          />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import Pengine from "../../modules/graphviz";
import ProgressSpinner from "primevue/progressspinner";

export default {
  components: { ProgressSpinner },
  data() {
    return {
      test: null,
      loading: false,
      pengine: null,
      selectedFrameIndex: 0,
      isNext: false,
    };
  },
  computed: {},
  methods: {
    next() {
      this.selectedFrameIndex++;
      this.isNext = false;
      this.renderGraph(this.test.sub_list[this.selectedFrameIndex].name);
    },
    renderGraph(sub) {
      return this.axios
        .post("/sapi/execcourse", {
          str: this.test.course.onto.cnl,
          sub: sub,
        })
        .then(
          (res) => {
            this.pengine.render(res.data).then(() => {
              this.pengine.randomNodes();
              this.pengine.randomEdges();
            });
            this.loading = true;
          },
          (error) => {
            console.log(error.response);
          }
        );
    },
    checkAnswers() {
      let result = this.pengine.checkAnswers();
      if (result !== false) {
        this.isNext = true;
        this.axios
          .put(
            `/api/users/courses/${this.$route.params.course}/tests/${this.$route.params.test}/answers`,
            {
              answer: result,
              sub: this.test.sub_list[this.selectedFrameIndex].name,
            }
          )
          .then(
            (res) => {
              console.log(res.data);
            },
            (error) => {
              console.log(error.response);
            }
          );
      }
    },
  },
  mounted() {
    this.$loadScript("/node_modules/viz.js/viz.js")
      .then(() => {
        this.$loadScript("/node_modules/viz.js/full.render.js")
          .then(() => {
            this.axios
              .get(`/api/users/courses/${this.$route.params.course}/tests`)
              .then((response) => {
                this.test = response.data;
                this.pengine = new Pengine();
                this.pengine.executeBinds();

                this.renderGraph(
                  this.test.sub_list[this.selectedFrameIndex].name
                );
              });
          })
          .catch(() => {
            // Failed to fetch script
          });
      })
      .catch(() => {
        // Failed to fetch script
      });
  },
};
</script>

<style>
#result {
  position: relative;
}

.node-menu {
  width: 100%;
  max-width: 300px;
  max-height: 400px;
  height: 77.7%;
  background: white;
  position: fixed;
  margin-top: 20px;
  margin-right: 10px;
  right: 0;
  top: 0;
  border: 2px solid #cccccc;
}

.node-menu ul {
  max-height: 270px;
  overflow: hidden;
  overflow-y: scroll;
}

ul {
  margin: 0;
  padding: 0;
}

.node-menu ul li {
  list-style: none;
  text-align: center;
  margin: 5px 0;
  padding: 7px 0;
  background: #f8f8f8;
}

.node-menu ul li:hover {
  background: #eeeeee;
  cursor: pointer;
}

.node-menu h2 {
  font-size: 20px;
  margin: 5px 0 15px;
  text-align: center;
}

.node-text,
.edge-text {
  font-size: 20px;
  cursor: pointer;
}

.edge-menu {
  width: 100%;
  max-width: 300px;
  max-height: 400px;
  height: 77.7%;
  background: white;
  position: fixed;
  margin-top: 20px;
  margin-right: 10px;
  right: 0;
  top: 0;
  border: 2px solid #cccccc;
  overflow: hidden;
  overflow-y: scroll;
}

.edge-menu ul li {
  list-style: none;
  text-align: center;
  margin: 5px 0;
  padding: 7px 0;
  background: #f8f8f8;
}

.edge-menu ul li:hover {
  background: #eeeeee;
  cursor: pointer;
}

.edge-menu h2 {
  font-size: 20px;
  margin: 5px 0 15px;
  text-align: center;
}

.remove-node {
  margin-top: 20px;
  background: #ff00007a;
  padding: 10px 0;
}

.remove-node:hover {
  cursor: pointer;
}
</style>