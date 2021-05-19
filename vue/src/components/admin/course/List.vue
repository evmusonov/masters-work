<template>
  <div class="p-grid">
    <ConfirmDialog></ConfirmDialog>
    <div class="p-col-12">
      <h2>Список курсов</h2>
      <Button
        @click="this.$router.push({ name: 'admin-courses-add' })"
        label="Создать"
        class="p-button-secondary"
      />
    </div>
    <div class="p-col-12">
      <DataTable :value="courses">
        <Column field="name" header="Название"></Column>
        <Column field="desc" header="Описание"></Column>
        <Column field="vis" header="Видимость">
          <template #body="slotProps">
            <span v-if="slotProps.data.vis">Опубликовано</span>
            <span v-else>Неопубликовано</span>
          </template>
        </Column>
        <Column header="Действия">
          <template #body="slotProps">
            <Button
              label="Изменить"
              class="p-mr-1 p-button-outlined"
              @click="
                this.$router.push(
                  '/admin/courses/' + slotProps.data._id + '/edit'
                )
              "
            />
            <Button
              label="Удалить"
              class="p-button-danger p-button-outlined"
              @click="this.delete(slotProps.data._id)"
            />
          </template>
        </Column>
      </DataTable>
    </div>
  </div>
</template>


<script>
import DataTable from "primevue/datatable";
import Column from "primevue/column";
//import ColumnGroup from "primevue/columngroup";

export default {
  components: { DataTable, Column },
  setup() {},
  data() {
    return {
      courses: null,
    };
  },
  computed: {},
  methods: {
    delete(uid) {
      this.$confirm.require({
        message: "Вы уверены, что хотите продолжить?",
        header: "Удаление курса",
        icon: "pi pi-exclamation-triangle",
        accept: () => {
          this.axios.delete(`/api/courses/${uid}`).then(() => {
            this.courses = this.courses.filter( el => el._id !== uid );
          });
        },
        reject: () => {
          
        },
      });
    },
  },
  mounted() {
    this.axios.get("/api/courses").then((res) => (this.courses = res.data));
  },
};
</script>