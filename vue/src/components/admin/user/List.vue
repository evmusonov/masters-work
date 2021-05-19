<template>
  <div class="p-grid">
    <ConfirmDialog></ConfirmDialog>
    <div class="p-col-12">
      <h2>Список пользователей</h2>
    </div>
    <div class="p-col-12">
      <DataTable :value="users">
        <Column field="_id" header="ID"></Column>
        <Column field="firstName" header="Имя"></Column>
        <Column field="surName" header="Фамилия"></Column>
        <Column field="email" header="Email"></Column>
        <Column field="role" header="Роль">
          <template #body="slotProps">
            <span>{{ roles[slotProps.data.role] }}</span>
          </template>
        </Column>
        <Column header="Действия">
          <template #body="slotProps">
            <Button
              label="Изменить"
              class="p-mr-1 p-button-outlined"
              @click="
                this.$router.push(
                  '/admin/users/' + slotProps.data._id + '/edit'
                )
              "
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
      users: null,
      roles: ['Заблокирован', 'Пользователь', 'Администратор', 'Редактор']
    };
  },
  computed: {},
  methods: {
  },
  mounted() {
    this.axios.get("/api/users").then((res) => (this.users = res.data));
  },
};
</script>