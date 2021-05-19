<template>
  <div class="p-grid">
    <ConfirmDialog :style="{ width: '50vw' }"></ConfirmDialog>
    <div class="p-col-6 p-text-center" v-if="course">
      <h1>
        Курс <span>"{{ course.name }}"</span>
      </h1>
      <div>Описание: {{ course.desc }}</div>
      <Button
        class="p-mt-3 p-button-rounded p-button-success"
        label="Начать тестирование"
        @click="startTesting"
      />
      <div class="p-grid">
        <div class="p-col-12">
          <Button
            class="p-mt-3 p-button-rounded p-button-danger"
            label="Покинуть курс"
            @click="leaveCourse"
          />
          <div>
            <ProgressSpinner
              class="p-mt-3"
              animationDuration=".6s"
              style="width: 50px; height: 50px"
              v-if="!loading"
            />
          </div>
        </div>
      </div>
    </div>
    <div class="p-col-6" v-if="course" :class="{ fixed: isListboxFixed }">
      <h3 v-if="!isListboxFixed">Выберите фрейм</h3>
      <div class="p-d-flex p-flex-column p-mb-3">
        <div class="p-text-right">
          <Button
            type="button"
            :icon="{
              'pi pi-angle-double-down': !showListbox,
              'pi pi-angle-double-up': showListbox,
            }"
            v-if="isListboxFixed"
            class="p-ml-auto"
            label="Фреймы"
            @click="showListbox = !showListbox"
          />
        </div>
        <div class="p-text-right">
          <Button
            class="p-mt-1 p-button-success"
            label="Полный граф"
            @click="showFullGraph"
          />
        </div>
      </div>
      <Listbox
        v-model="selectedFrame"
        :options="course.onto.sub_list"
        optionLabel="name"
        :filter="true"
        @change="show"
        listStyle="max-height:250px"
        v-if="!isListboxFixed || (showListbox && isListboxFixed)"
      />
    </div>
    <div class="p-col-12 p-text-center" v-if="course">
      <div id="result"></div>
    </div>
  </div>
</template>

<script>
import Pengine from "../../modules/graphviz";
import ProgressSpinner from "primevue/progressspinner";
import Listbox from "primevue/listbox";

export default {
  components: { ProgressSpinner, Listbox },
  data() {
    return {
      selectedSubIndex: 0,
      course: null,
      onto: null,
      loading: false,
      favCourse: false,
      selectedFrame: null,
      scrollPosition: null,
      showListbox: false,
      pengine: null,
    };
  },
  computed: {
    isListboxFixed() {
      if (this.scrollPosition >= 60) {
        return true;
      } else {
        return false;
      }
    },
  },
  methods: {
    showFullGraph() {
      this.axios
        .post("/sapi/exec", {
          str: this.course.onto.cnl,
        })
        .then(
          (res) => {
            this.pengine.render(res.data);
            this.loading = true;
          },
          (error) => {
            console.log(error.response);
          }
        );
    },
    updateScroll() {
      this.scrollPosition = window.scrollY;
    },
    show() {
      this.renderGraph(this.selectedFrame.name);
    },
    renderGraph(sub) {
      this.axios
        .post("/sapi/execcourse", {
          str: this.course.onto.cnl,
          sub: sub,
        })
        .then(
          (res) => {
            this.pengine.render(res.data);
            this.loading = true;
          },
          (error) => {
            console.log(error.response);
          }
        );
    },
    leaveCourse() {
      this.$confirm.require({
        message: "Вы уверены, что хотите покинуть курс?",
        header: "Выход из курса",
        icon: "pi pi-exclamation-triangle",
        accept: () => {
          this.axios
            .delete("/api/users/courses/" + this.$route.params.uid)
            .then(() => {
              this.$router.push(`/courses/${this.$route.params.uid}`);
            });
        },
        reject: () => {},
      });
    },
    startTesting() {
      this.$confirm.require({
        message:
          "Начиная тестирование, вы не сможете просматривать материал курса до окончания теста. Вы уверены, что хотите начать?",
        header: "Предупреждение",
        icon: "pi pi-exclamation-triangle",
        accept: () => {
          this.axios
            .post("/api/users/courses/" + this.$route.params.uid + "/tests", {
              course_uid: this.$route.params.uid,
              sub_list: this.course.onto.sub_list,
            })
            .then((res) => {
              this.$router.push(
                `/courses/${this.$route.params.uid}/tests/${res.data._id}`
              );
            });
        },
        reject: () => {},
      });
    },
  },
  mounted() {
    window.addEventListener("scroll", this.updateScroll);
    this.axios
      .get("/api/courses/" + this.$route.params.uid, {
        params: { where: { vis: 1, del: 0 } },
      })
      .then((response) => {
        this.course = response.data;

        let checkTests = this.$store.getters.getUser.tests.find((element) => {
          return element.course == this.$route.params.uid;
        });
        if (checkTests !== undefined) {
          this.$router.push(
            `/courses/${this.$route.params.uid}/tests/${checkTests._id}`
          );
        }

        this.pengine = new Pengine();
        this.pengine.executeBinds();

        this.renderGraph(this.course.onto.sub_list[this.selectedSubIndex].name);
      });
  },
  watch: {
    $route() {
      if (this.$store.getters.isAuth) {
        this.$store.dispatch("setUserFromDb");
      }
    },
  },
};
</script>

<style>
.fixed {
  position: fixed;
  top: 0px;
  right: 0px;
  max-width: 350px;
  z-index: 1;
}
</style>