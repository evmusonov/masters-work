<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Регистрация</h1>
    </div>
    <div class="p-col-4 p-offset-4">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field p-grid">
            <label for="email" class="p-col-12 p-mb-2 p-md-2 p-mb-md-0"
              >Email</label
            >
            <div class="p-col-12 p-md-10">
              <InputText
                v-model="form.body.email"
                id="email"
                type="text"
              />
              <small class="p-error">{{ form.errors.email }}</small>
            </div>
          </div>
          <div class="p-field p-grid">
            <label for="password" class="p-col-12 p-mb-2 p-md-2 p-mb-md-0"
              >Пароль</label
            >
            <div class="p-col-12 p-md-10">
              <InputText
                v-model="form.body.password"
                id="password"
                type="password"
              />
              <small class="p-error">{{ form.errors.password }}</small>
            </div>
          </div>
          <Button @click="registerDefault" label="Регистрация" class="p-button-secondary" />
        </div>
      </div>
    </div>
  </div>
</template>


<script>
export default {
  components: {},
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
      this.axios.post("/api/users/register", this.form.body).then(
        () => {
          this.form.errors = {};
          this.$toast.add({
            severity: "success",
            summary: "Вы зарегистрированы",
            life: 3000,
          });
          this.$router.push("/login");
        },
        (error) => {
          this.errors(error.response.data);
        }
      );
    },
  },
};
</script>