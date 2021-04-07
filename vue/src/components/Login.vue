<template>
  <div class="text-center">
    <h1>Логин</h1>
    <br />

    <div class="input-group">
      <InputText v-model="form.body.email" placeholder="Email" type="text" />

      <div>{{ form.errors.email }}</div>
    </div>

    <br />

    <div class="input-group">
      <InputText
        v-model="form.body.password"
        placeholder="Пароль"
        type="password"
      />

      <div>{{ form.errors.password }}</div>
    </div>

    <br />
    <Button @click="login" label="Войти" class="p-button-secondary" />
  </div>
</template>


<script>

export default {
  components: {
  },
  setup() {},
  data() {
    return {
      form: {
        body: {
          email: "",
          password: "",
        },
        errors: {},
      },
    };
  },
  methods: {
    errors(err) {
      this.form.errors = Object.fromEntries(
        err.errors.map((item) => [item.param, item.msg])
      );
    },
    login() {
      this.axios.post("/api/user/login", this.form.body).then(
        (res) => {
          this.form.errors = {};
          this.$store.commit({
            type: "setToken",
            tokenType: "accessToken",
            value: res.data.accessToken,
          });
          this.$store.commit({
            type: "setToken",
            tokenType: "refreshToken",
            value: res.data.refreshToken,
          });
          this.$store.commit({
            type: "setUser",
            user: res.data.user,
          });

          this.$toast.add({
            severity: "success",
            summary: "Вы успешно авторизованы",
            //detail: "Order submitted",
            life: 3000,
          });
          this.$router.push("/user");
        },
        (error) => {
          this.errors(error.response.data);
        }
      );
    },
  },
};
</script>