<template>
  <div class="p-grid">
    <ConfirmDialog></ConfirmDialog>
    <div class="p-col-12">
      <h2>Список онтологий</h2>
      <Button
        @click="this.$router.push({ name: 'admin-ontos-add' })"
        label="Создать"
        class="p-button-secondary"
      />
    </div>
    <div class="p-col-12">
      <DataTable :value="ontos">
        <Column field="name" header="Название"></Column>
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
                  '/admin/ontos/' + slotProps.data._id + '/edit'
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
      ontos: null,
    };
  },
  computed: {},
  methods: {
    delete(uid) {
      this.$confirm.require({
        message: "Вы уверены, что хотите продолжить?",
        header: "Удаление онтологии",
        icon: "pi pi-exclamation-triangle",
        accept: () => {
          this.axios.delete(`/api/ontos/${uid}`).then(() => {
            this.ontos = this.ontos.filter( el => el._id !== uid );
          });
        },
        reject: () => {
          
        },
      });
    },
  },
  mounted() {
    this.axios.get("/api/ontos").then((res) => (this.ontos = res.data));
  },
};
</script>