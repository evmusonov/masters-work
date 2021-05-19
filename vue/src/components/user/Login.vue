<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Авторизация</h1>
    </div>
    <div class="p-col-4 p-offset-4">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field p-grid">
            <label for="email" class="p-col-12 p-mb-2 p-md-2 p-mb-md-0"
              >E-mail</label
            >
            <div class="p-col-12 p-md-10">
              <InputText
                v-model="this.form.body.email"
                id="email"
                type="text"
              />
              <small class="p-error">{{ this.form.errors.email }}</small>
            </div>
          </div>
          <div class="p-field p-grid">
            <label for="password" class="p-col-12 p-mb-2 p-md-2 p-mb-md-0"
              >Пароль</label
            >
            <div class="p-col-12 p-md-10">
              <InputText
                v-model="this.form.body.password"
                id="password"
                type="password"
              />
              <small class="p-error">{{ this.form.errors.password }}</small>
            </div>
          </div>
          <Button @click="login" label="Войти" class="p-button-secondary" />
        </div>
      </div>
    </div>
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
      this.axios.post("/api/users/login", this.form.body).then(
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