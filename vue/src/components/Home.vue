<template>
  <div class="p-grid">
    <div class="p-col-12 p-text-center">
      <h1>Доступные курсы</h1>
    </div>
    <div class="p-col-12">
      <div class="card">
        <DataView
          :value="courses"
          :layout="layout"
          :paginator="true"
          :rows="9"
          :sortOrder="sortOrder"
          :sortField="sortField"
        >
          <template #grid="slotProps">
            <div style="padding: 0.5em" class="p-col-12 p-md-3 p-text-center">
              <Button
                @click="this.$router.push(`/courses/${slotProps.data._id}`)"
                :label="slotProps.data.name"
                class="p-button-link"
              />
              <small class="p-d-block">{{ slotProps.data.desc }}</small>
            </div>
          </template>
        </DataView>
      </div>
    </div>
  </div>
</template>

<script>
import DataView from "primevue/dataview";

export default {
  name: "Home",
  components: { DataView },
  data() {
    return {
      courses: [],
      layout: "grid",
      sortKey: null,
      sortOrder: null,
      sortField: null,
      sortOptions: [
        { label: "Price High to Low", value: "!price" },
        { label: "Price Low to High", value: "price" },
      ],
    };
  },
  methods: {
    onSortChange(event) {
      const value = event.value.value;
      const sortValue = event.value;

      if (value.indexOf("!") === 0) {
        this.sortOrder = -1;
        this.sortField = value.substring(1, value.length);
        this.sortKey = sortValue;
      } else {
        this.sortOrder = 1;
        this.sortField = value;
        this.sortKey = sortValue;
      }
    },
  },
  mounted() {
    this.axios
      .get("/api/courses", { params: { where: { vis: 1, del: 0 } } })
      .then((response) => (this.courses = response.data));
  },
};
</script>