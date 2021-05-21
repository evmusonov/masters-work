<template>
  <div class="p-grid">
    <div class="p-col-12">
      <h2>Мои активные курсы</h2>
      <div v-if="subCourses.length == 0">У вас пока нет активных курсов</div>
      <DataTable :value="subCourses">
        <Column field="name" header="Название"></Column>
        <Column field="desc" header="Описание"></Column>
        <Column header="Действия">
          <template #body="slotProps">
            <Button
              label="Перейти к курсу"
              class="p-mr-1 p-button-outlined"
              @click="
                this.$router.push(
                  '/courses/study/' + slotProps.data._id
                )
              "
            />
            <Button
              label="Прогресс"
              class="p-mr-1 p-button-outlined p-button-success"
              @click="
                this.$router.push(
                  '/user/courses/' + slotProps.data._id
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

export default {
  components: { DataTable, Column },
  setup() {},
  data() {
    return {
      subCourses: [],
    };
  },
  computed: {},
  methods: {
    
  },
  mounted() {
    this.axios.get("/api/users/courses").then((res) => {
      this.subCourses = res.data;
    });
  },
};
</script>