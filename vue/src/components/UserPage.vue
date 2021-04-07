<template>
  <div class="p-grid">
    <h1>Привет, {{ this.$store.getters.getUser.firstName }}!</h1>
  </div>
</template>


<script>
export default {
  components: {},
  setup() {},
  data() {
    return {};
  },
  methods: {
    checkDataFilling() {
      if (!this.$store.getters.getUser.firstName) {
        this.$toast.add({
          severity: "info",
          summary: "Необходимо заполнить профиль для дальнейшей работы",
          life: 5000,
        });
        this.$router.push("/user/settings");
      }
    },
  },
  mounted() {
    this.checkDataFilling()
    // setTimeout(() => {
    //   if (!this.$store.getters.getUser.firstName) {
    //     this.$toast.add({
    //       severity: "info",
    //       summary: "Необходимо заполнить профиль для дальнейшей работы",
    //       life: 5000,
    //     });
    //     this.$router.push("/user/settings");
    //   }
    // }, 100);
  },
};
</script>