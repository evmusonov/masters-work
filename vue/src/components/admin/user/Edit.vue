<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h2>Редактирование пользователя</h2>
    </div>
    <div class="p-col-8 p-offset-2">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field">
            <label for="name">Имя</label>
            <InputText
              v-model="form.body.firstName"
              id="firstName"
              type="text"
            />
            <small class="p-error">{{ form.errors.firstName }}</small>
          </div>
          <div class="p-field">
            <label for="desc">Фамилия</label>
            <InputText v-model="form.body.surName" id="firstName" type="text" />
            <small class="p-error">{{ form.errors.surName }}</small>
          </div>
          <div class="p-field">
            <label for="desc">Email</label>
            <InputText v-model="form.body.email" id="email" type="text" />
            <small class="p-error">{{ form.errors.email }}</small>
          </div>
          <div class="p-field">
            <label for="onto">Роль</label>
            <Dropdown
              v-model="selectedRole"
              :options="roles"
              optionLabel="name"
              placeholder="Выберите роль"
            />
            <small class="p-error">{{ form.errors.role }}</small>
          </div>
          <Button @click="edit" label="Изменить" class="p-button-secondary" />
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
      selectedRole: {},
      roles: [
        { name: "Заблокирован" },
        { name: "Пользователь" },
        { name: "Администратор" },
        { name: "Редактор" }
      ],
      form: {
        body: {
          name: "",
          desc: "",
          vis: false,
          onto_uid: null,
        },
        errors: {},
      },
    };
  },
  computed: {},
  methods: {
    errors(err) {
      this.form.errors = Object.fromEntries(
        err.errors.map((item) => [item.param, item.msg])
      );
    },
    edit() {
      this.axios
        .put(`/api/courses/${this.$route.params.uid}`, this.form.body)
        .then(
          () => {
            this.form.errors = {};
            this.$toast.add({
              severity: "success",
              summary: "Курс изменен",
              life: 3000,
            });
            this.$router.push("/admin/courses");
          },
          (error) => {
            this.errors(error.response.data);
          }
        );
    },
  },
  mounted() {
    this.axios.get(`/api/users/${this.$route.params.uid}`).then((res) => {
      this.form.body = res.data;
      this.selectedRole = this.roles[this.form.body.role];
    });
  },
};
</script>