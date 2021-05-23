<template>
  <div>
    <Toast />
    <Menubar :model="items">
      <template #start>
        <div class="p-mr-2">Обучающая система СГУПС</div>
      </template>
      <template #end>
        <InputText placeholder="Search" type="text" />
      </template>
    </Menubar>
    <i v-if="!isAuth" class="pi pi-spin pi-spinner" style="fontsize: 2rem"></i>
    <router-view v-if="isAuth"></router-view>
  </div>
</template>

<script>
export default {
  name: "LayoutDefault",

  data() {
    return {
      leftDrawerOpen: false,
      userLoaded: false,
      items: [
        {
          label: "Главная",
          to: "/",
        },
        {
          label: "Регистрация",
          icon: "pi pi-fw pi-user-plus",
          to: "/register",
          visible: () => {
            return !this.$store.getters.isAuth;
          },
        },
        {
          label: "Вход",
          icon: "pi pi-fw pi-sign-in",
          to: "/login",
          visible: () => {
            return !this.$store.getters.isAuth;
          },
        },
        {
          label: "Профиль",
          visible: () => {
            return this.$store.getters.isAuth;
          },
          items: [
            {
              label: "Профиль",
              icon: "pi pi-fw pi-user",
              to: "/user",
            },
            {
              label: "Настройки",
              icon: "pi pi-fw pi-cog",
              to: "/user/settings",
            },
          ],
        },
        {
          label: "Панель управления",
          visible: () => {
            return this.$store.getters.getUser.role == 2;
          },
          to: "/admin",
        },
        {
          label: "Выход",
          icon: "pi pi-fw pi-sign-out",
          to: "/",
          command: () => {
            this.$store.commit({
              type: "setToken",
              tokenType: "accessToken",
              value: "",
            });
            this.$store.commit({
              type: "setToken",
              tokenType: "refreshToken",
              value: "",
            });
            this.$store.commit({
              type: "setUser",
              user: {},
            });
          },
          visible: () => {
            return this.$store.getters.isAuth;
          },
        },
      ],
    };
  },
  methods: {
    checkDataFilling() {
      if (!this.$store.getters.getUser.firstName) {
        this.$toast.add({
          severity: "info",
          summary: "Необходимо заполнить профиль для дальнейшей работы",
          life: 5000,
        });
        this.$router.push("/user/settings");
      }
    },
  },
  computed: {
    isAuth() {
      return this.$store.getters.isLogged;
    },
    isUserLoaded() {
      return this.userLoaded;
    },
  },
  mounted() {
    this.$store.commit({
      type: "setTokenFromStorage",
      tokenType: "accessToken",
    });
    this.$store.commit({
      type: "setTokenFromStorage",
      tokenType: "refreshToken",
    });
    if (this.$store.getters.isAuth) {
      this.$store.dispatch("setUserFromDb").then(() => this.userLoaded = true);
    }
  },
  watch: {
    $route() {
      if (
        this.isUserLoaded &&
        this.$store.getters.isAuth &&
        this.$route.name != "user-settings"
      ) {
        this.checkDataFilling();
      }
    },
  },
};
</script>

<style>
@import url("https://fonts.googleapis.com/css2?family=Raleway:wght@400&display=swap");

#app {
  font-family: "Raleway", sans-serif !important;
}
</style>
