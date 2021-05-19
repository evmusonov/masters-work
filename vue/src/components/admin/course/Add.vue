<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Добавление курса</h1>
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
            <Textarea v-model="form.body.desc" :autoResize="true" rows="5"/>
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
          <Button @click="add" label="Создать" class="p-button-secondary" />
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
    add() {
      this.form.body.onto_uid = this.selectedOnto._id;
      this.axios.post("/api/courses", this.form.body).then(
        () => {
          this.form.errors = {};
          this.$toast.add({
            severity: "success",
            summary: "Курс добавлен",
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
    this.axios
      .get("/api/ontos", { params: { where: { vis: 1, del: 0 } } })
      .then((res) => (this.ontos = res.data));
  },
};
</script>