<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Настройки</h1>
    </div>
    <div class="p-col-4 p-offset-4">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field p-grid">
            <label for="firstname" class="p-col-12 p-mb-2 p-md-2 p-mb-md-0"
              >Имя</label
            >
            <div class="p-col-12 p-md-10">
              <InputText
                v-model="this.$store.getters.getUser.firstName"
                id="firstname"
                type="text"
              />
              <small class="p-error">{{ this.form.errors.firstName }}</small>
            </div>
          </div>
          <div class="p-field p-grid">
            <label for="surname" class="p-col-12 p-mb-2 p-md-2 p-mb-md-0"
              >Фамилия</label
            >
            <div class="p-col-12 p-md-10">
              <InputText
                v-model="this.$store.getters.getUser.surName"
                id="surname"
                type="text"
              />
              <small class="p-error">{{ this.form.errors.surName }}</small>
            </div>
          </div>
          <Button @click="save" label="Сохранить" class="p-button-secondary" />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  created() {
    console.log(this.$store.getters.getUser);
  },
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
      this.form.body.firstName = this.$store.getters.getUser.firstName;
      this.form.body.surName = this.$store.getters.getUser.surName;
      this.axios
        .put("/api/user/" + this.$store.getters.getUser._id, this.form.body)
        .then(
          () => {
            this.form.errors = {};
          },
          (error) => {
            this.errors(error.response.data);
          }
        );
    },
  },
};
</script>