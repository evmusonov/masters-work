<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Редактирование онтологии</h1>
    </div>
    <div class="p-col-8 p-offset-2">
      <div class="p-card p-p-2">
        <div class="p-fluid">
          <div class="p-field">
            <label for="name">Название онтологии</label>
            <InputText v-model="form.body.name" id="name" type="text" />
            <small class="p-error">{{ form.errors.name }}</small>
          </div>
          <div class="p-field">
            <TabView>
              <TabPanel header="КЕЯ"><div id="cnl_editor"></div></TabPanel>
            </TabView>
          </div>
          <div class="p-field">
            <label for="vis">Видимость</label>
            <Checkbox
              class="p-d-flex"
              id="vis"
              v-model="form.body.vis"
              :binary="true"
            />
          </div>
          <Button @click="edit" label="Сохранить" class="p-button-secondary" />
        </div>
      </div>
    </div>
  </div>
</template>


<script>
import editorModule from "../../../modules/editor";

export default {
  components: {},
  setup() {},
  data() {
    return {
      cnlEditor: null,
      form: {
        body: {
          name: "",
          vis: false,
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
      // this.codeMirror.dispatch({
      //   changes: { from: 0, insert: "#!/usr/bin/env node\n" },
      // });

      this.form.body.cnl = this.cnlEditor.state.doc.toString();

      this.axios.post("/sapi/execsub", { str: this.form.body.cnl }).then(
        (res) => {
          console.log(res.data);
          this.form.body.sub_list = res.data;

          this.axios
            .put(`/api/ontos/${this.$route.params.uid}`, this.form.body)
            .then(
              () => {
                this.form.errors = {};
                this.$toast.add({
                  severity: "success",
                  summary: "Онтология изменена",
                  life: 3000,
                });
                this.$router.push("/admin/ontos");
              },
              (error) => {
                this.errors(error.response.data);
              }
            );
        },
        (error) => {
          console.log(error.response);
        }
      );
    },
  },
  mounted() {
    this.cnlEditor = editorModule(document.getElementById("cnl_editor"));

    this.axios.get(`/api/ontos/${this.$route.params.uid}`).then((res) => {
      this.form.body = res.data;
      this.cnlEditor.dispatch({
        changes: { from: 0, insert: res.data.cnl },
      });
    });
  },
};
</script>