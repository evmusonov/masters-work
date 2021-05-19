<template>
  <div class="p-grid nested-grid p-mt-1">
    <div class="p-col-12">
      <h1>Панель управления</h1>
      <MegaMenu :model="items" />
    </div>
    <Course v-if="this.showIfRouterContains('admin-courses')" />
    <Onto v-if="this.showIfRouterContains('admin-ontos')" />
    <Main v-if="this.showIfRouterContains('admin-main')" />
    <User v-if="this.showIfRouterContains('admin-users')" />
  </div>
</template>

<script>
import MegaMenu from "primevue/megamenu";
import Course from "./course/Course.vue";
import Onto from "./onto/Onto.vue";
import Main from "./main/Main.vue";
import User from "./user/User.vue";

export default {
  components: { MegaMenu, Course, Onto, Main, User },
  setup() {},
  data() {
    return {
      items: [
        {
          label: "Главная",
          to: "/admin",
          //icon: "pi pi-fw pi-video",
        },
        {
          label: "Пользователи",
          to: "/admin/users",
          //icon: "pi pi-fw pi-video",
        },
        {
          label: "Курсы",
          to: "/admin/courses",
          //icon: "pi pi-fw pi-video",
        },
        {
          label: "Онтологии",
          to: "/admin/ontos",
          //icon: "pi pi-fw pi-video",
        },
      ],
    };
  },
  methods: {
    showIfRouterContains(routeName) {
      return this.$route.name.indexOf(routeName) != -1;
    }
  },
};
</script>