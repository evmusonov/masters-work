<template>
  <div class="p-col-12">
    <List v-if="this.$route.name == 'admin-ontos'" />
    <Add v-if="this.$route.name == 'admin-ontos-add'" />
    <Edit v-if="this.$route.name == 'admin-ontos-edit'" />
  </div>
</template>


<script>
import List from "./List.vue";
import Add from "./Add.vue";
import Edit from "./Edit.vue";

export default {
  components: { List, Add, Edit },
  setup() {},
  data() {
    return {};
  },
  computed: {},
  methods: {},
};
</script>