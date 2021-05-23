<template>
  <div class="p-grid">
    <ConfirmDialog></ConfirmDialog>
    <div class="p-col-12 p-text-center" v-if="course">
      <h1>
        Прогресс курса <span>"{{ course.name }}"</span>
      </h1>
      <div>Описание: {{ course.desc }}</div>
      <div>
        <ProgressSpinner
          class="p-mt-3"
          animationDuration=".6s"
          style="width: 50px; height: 50px"
          v-if="!loading"
        />
      </div>
      <div id="result"></div>
    </div>
  </div>
</template>

<script>
import Pengine from "../../modules/graphviz";
import GraphProgress from "../../modules/graphProgress";
import ProgressSpinner from "primevue/progressspinner";

export default {
  components: { ProgressSpinner },
  data() {
    return {
      course: null,
      onto: null,
      loading: false,
      pengine: null,
      test: null,
      progress: null,
    };
  },
  methods: {},
  mounted() {
    this.$loadScript("/node_modules/viz.js/viz.js")
      .then(() => {
        this.$loadScript("/node_modules/viz.js/full.render.js")
          .then(() => {
            this.axios
              .get("/api/courses/" + this.$route.params.uid, {
                params: { where: { vis: 1, del: 0 } },
              })
              .then((response) => {
                this.course = response.data;

                this.axios
                  .post("/sapi/exec", { str: this.course.onto.cnl })
                  .then(
                    (res) => {
                      this.pengine = new Pengine();
                      this.pengine.executeBinds();
                      this.pengine.render(res.data);
                      this.loading = true;

                      this.axios
                        .get(
                          `/api/users/courses/${this.$route.params.uid}/tests`
                        )
                        .then((res) => {
                          this.test = res.data;
                          this.progress = new GraphProgress();
                          this.progress.check(this.test.answers);
                        });
                    },
                    (error) => {
                      console.log(error.response);
                    }
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