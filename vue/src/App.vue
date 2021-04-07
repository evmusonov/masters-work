<template>
  <div>
    <Toast />
    <Menubar :model="items" />
    <router-view></router-view>
  </div>
</template>

<script>

export default {
  name: "LayoutDefault",

  data() {
    return {
      leftDrawerOpen: false,
      items: [
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
          label: "Запрос",
          command: () => {
            this.axios.get("/api/users");
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
          label: "Админ панель",
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
  },
  created() {
    this.$store.commit({
      type: "setTokenFromStorage",
      tokenType: "accessToken",
    });
    this.$store.commit({
      type: "setTokenFromStorage",
      tokenType: "refreshToken",
    });
    if (this.$store.getters.isAuth) {
      this.$store.dispatch('setUserFromDb');
    }
  },
};
</script>

<style>
#app {
}
</style>
