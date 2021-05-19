<template>
  <div class="p-grid">
    <ConfirmDialog></ConfirmDialog>
    <div class="p-col-12 p-text-center" v-if="course">
      <h1>
        Курс <span>"{{ course.name }}"</span>
        <Button
          class="p-ml-1 p-button-rounded p-button-text"
          label="Добавить в избранное"
          icon="pi pi-heart"
          iconPos="right"
          @click="addToFavorite"
          v-if="!favCourse"
        />
        <Button
          class="p-ml-1 p-button-rounded p-button-text p-button-secondary"
          label="Удалить из избранного"
          icon="pi pi-heart"
          iconPos="right"
          @click="delFromFavorite"
          v-if="favCourse"
        />
      </h1>
      <div>Описание: {{ course.desc }}</div>
      <Button
        class="p-mt-2 p-button-rounded p-button-secondary"
        label="Начать курс"
        @click="startCourse"
        v-if="!this.courseStarted"
      />
      <Button
        class="p-mt-2 p-button-rounded p-button-secondary"
        label="Продолжить изучение курса"
        @click="this.$router.push(`/courses/study/${this.$route.params.uid}`)"
        v-if="this.courseStarted"
      />
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
//import Pengine from "./../../modules/pengines";
import Pengine from "../../modules/graphviz";
import ProgressSpinner from "primevue/progressspinner";

export default {
  components: { ProgressSpinner },
  data() {
    return {
      course: null,
      onto: null,
      loading: false,
      favCourse: false,
      courseStarted: false,
      pengine: null,
    };
  },
  methods: {
    startCourse() {
      this.$confirm.require({
        message: "Вы уверены, что хотите начать изучение данного курса?",
        header: "Начало курса",
        icon: "pi pi-exclamation-triangle",
        accept: () => {
          this.axios
            .post("/api/users/courses/" + this.$route.params.uid)
            .then(() => {
              this.$router.push(`/courses/study/${this.$route.params.uid}`);
            });
        },
        reject: () => {},
      });
    },
    addToFavorite() {
      this.axios
        .post("/api/users/favs/courses/" + this.$route.params.uid)
        .then(() => {
          this.favCourse = true;
        });
    },
    delFromFavorite() {
      this.axios
        .delete("/api/users/favs/courses/" + this.$route.params.uid)
        .then(() => {
          this.favCourse = false;
        });
    },
    checkUserSubscription() {
      if (
        Object.values(this.course.subUsers).includes(
          this.$store.getters.getUser._id
        )
      ) {
        this.courseStarted = true;
      }
    },
  },
  mounted() {
    this.axios
      .get("/api/courses/" + this.$route.params.uid, {
        params: { where: { vis: 1, del: 0 } },
      })
      .then((response) => {
        this.course = response.data;
        this.checkUserSubscription();

        this.axios.post("/sapi/exec", { str: this.course.onto.cnl }).then(
          (res) => {
            this.pengine = new Pengine();
            this.pengine.executeBinds();

            this.pengine.render(res.data);
            this.loading = true;
          },
          (error) => {
            console.log(error.response);
          }
        );

        this.axios
          .get("/api/users/favs/courses/" + this.$route.params.uid)
          .then((res) => {
            this.favCourse = res.data.fav;
          });
      });
  },
};
</script>