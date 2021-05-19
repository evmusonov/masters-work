<template>
  <div class="p-col-12">
    <List v-if="this.$route.name == 'admin-users'" />
    <Edit v-if="this.$route.name == 'admin-users-edit'" />
  </div>
</template>


<script>
import List from "./List.vue";
import Edit from "./Edit.vue";

export default {
  components: { List, Edit },
  setup() {},
  data() {
    return {};
  },
  computed: {},
  methods: {},
};
</script>