<template>
  <div class="text-center">
    <h1>Регистрация</h1>
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

    <Button
      @click="registerDefault"
      label="Регистрация"
      class="p-button-secondary"
    />
    <li v-for="err in form.err" v-bind:key="err">
      {{ err }}
    </li>
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
    registerDefault() {
      this.axios.post("/api/user/register", this.form.body).then(
        () => {
          this.form.errors = {};
          this.$toast.add({
            severity: "success",
            summary: "Вы зарегистрированы",
            life: 3000,
          });
          this.$router.push('/login');
        },
        (error) => {
          this.errors(error.response.data);
        }
      );
    },
  },
};
</script>