<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Редактирование курса</h1>
    </div>
    <div class="p-col-8 p-offset-2">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field">
            <label for="name">Название курса</label>
            <InputText v-model="form.body.name" id="name" type="text" />
            <small class="p-error">{{ form.errors.name }}</small>
          </div>
          <div class="p-field">
            <label for="desc">Описание</label>
            <Textarea v-model="form.body.desc" :autoResize="true" rows="5" />
            <small class="p-error">{{ form.errors.desc }}</small>
          </div>
          <div class="p-field">
            <label for="onto">Онтология</label>
            <Dropdown
              v-model="selectedOnto"
              :options="ontos"
              optionLabel="name"
              placeholder="Выберите онтологию"
            />
            <small class="p-error">{{ form.errors.onto_uid }}</small>
          </div>
          <div class="p-field">
            <label for="vis">Видимость</label>
            <Checkbox id="vis" v-model="form.body.vis" :binary="true" />
            <small class="p-error">{{ form.errors.vis }}</small>
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
      selectedOnto: {},
      ontos: [],
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
    this.axios.get(`/api/courses/${this.$route.params.uid}`).then((res) => {
      this.form.body = res.data;
      this.axios
        .get("/api/ontos", { params: { where: { vis: 1, del: 0 } } })
        .then((res) => {
          this.ontos = res.data;
          this.selectedOnto = this.ontos.find(x => x._id === this.form.body.onto._id);
        });
    });
  },
};
</script>