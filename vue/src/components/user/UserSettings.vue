<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Настройки</h1>
    </div>
    <div class="p-col-8 p-offset-2" v-if="this.$store.getters.getUser.firstName">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field">
            <label for="firstname">Имя</label>
            <InputText
              v-model="this.form.body.firstName"
              id="firstname"
              type="text"
            />
            <small class="p-error">{{ this.form.errors.firstName }}</small>
          </div>
          <div class="p-field">
            <label for="surname">Фамилия</label>
            <InputText
              v-model="this.form.body.surName"
              id="surname"
              type="text"
            />
            <small class="p-error">{{ this.form.errors.surName }}</small>
          </div>
          <Button @click="save" label="Сохранить" class="p-button-secondary" />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      form: {
        body: {
          firstName: "",
          surName: "",
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
    save() {
      // this.form.body.firstName = this.$store.getters.getUser.firstName;
      // this.form.body.surName = this.$store.getters.getUser.surName;
      this.axios
        .put("/api/user/" + this.$store.getters.getUser._id, this.form.body)
        .then(
          () => {
            this.form.errors = {};
            this.$toast.add({
              severity: "success",
              summary: "Данные успешно изменены",
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
  mounted() {
    this.form.body = this.$store.getters.getUser;
  },
};
</script>