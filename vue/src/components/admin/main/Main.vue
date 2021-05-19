<template>
  <div class="p-col-12">
    <div class="p-grid">
      <div class="p-col-12">
        <h2>Общая информация</h2>
      </div>
      <div class="p-col-4">
        <Card v-if="users">
          <template #title>Пользователи</template>
          <template #content>
            <div>Всего: {{ users.length }}</div>
            <Divider />
            <div>Неподтвержденные: {{ unverifiedUsers }}</div>
            <Divider />
            <div>Администраторы: {{ admins }}</div>
            <Divider />
            <div>Редактора: {{ editors }}</div>
          </template>
          <template #footer> </template>
        </Card>
      </div>
      <div class="p-col-4">
        <Card v-if="courses">
          <template #title>Курсы</template>
          <template #content>
            <div>Всего: {{ courses.length }}</div>
            <Divider />
            <div>
              Активные:
              {{
                courses.filter((item) => item.del == 0 && item.vis == 1).length
              }}
            </div>
            <Divider />
            <div>
              Удаленные:
              {{ courses.filter((item) => item.del == 1).length }}
            </div>
            <Divider />
            <div>
              Неактивные:
              {{
                courses.filter((item) => item.del == 0 && item.vis == 0).length
              }}
            </div>
            <Divider />
          </template>
          <template #footer> </template>
        </Card>
      </div>
      <div class="p-col-4">
        <Card v-if="ontos">
          <template #title>Онтологии</template>
          <template #content>
            <div>Всего: {{ ontos.length }}</div>
            <Divider />
            <div>
              Активные:
              {{
                ontos.filter((item) => item.del == 0 && item.vis == 1).length
              }}
            </div>
            <Divider />
            <div>
              Удаленные:
              {{ ontos.filter((item) => item.del == 1).length }}
            </div>
            <Divider />
            <div>
              Неактивные:
              {{
                ontos.filter((item) => item.del == 0 && item.vis == 0).length
              }}
            </div>
            <Divider />
          </template>
          <template #footer> </template>
        </Card>
      </div>
    </div>
  </div>
</template>


<script>
import Card from "primevue/card";
import Divider from "primevue/divider";

export default {
  components: { Card, Divider },
  data() {
    return {
      users: null,
      courses: null,
      ontos: null,
    };
  },
  computed: {
    unverifiedUsers() {
      return this.users.filter((item) => item.role == 0).length;
    },
    admins() {
      return this.users.filter((item) => item.role == 2).length;
    },
    editors() {
      return this.users.filter((item) => item.role == 3).length;
    },
  },
  methods: {},
  mounted() {
    this.axios.get("/api/users").then((res) => {
      this.users = res.data;
    });
    this.axios.get("/api/courses", { params: { where: {} } }).then((res) => {
      this.courses = res.data;
    });
    this.axios.get("/api/ontos", { params: { where: {} } }).then((res) => {
      this.ontos = res.data;
    });
  },
};
</script>