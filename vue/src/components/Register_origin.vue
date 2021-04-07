<template>
  <div class="text-center">
    <br />

    <div class="input-group">
      <input v-model="form.body.email" placeholder="Email" type="text" />

      <div>{{ form.errors.email }}</div>
    </div>

    <br />

    <div class="input-group">
      <input
        v-model="form.body.password"
        placeholder="Password"
        type="password"
      />

      <div>{{ form.errors.password }}</div>
    </div>

    <br />

    <button @click="registerDefault">Default</button>
  </div>
</template>


<script>
import { useToast } from "vue-toastification";

export default {
  setup() {
    // Get toast interface
    const toast = useToast();

    // Use it!
    //toast("I'm a toast!");

    // or with options
    // toast.success("My toast content", {
    //   timeout: 2000
    // });
    // These options will override the options defined in the "app.use" plugin registration for this specific toast

    // Make it available inside methods
    return { toast }
  },
  data() {
    return {
      form: {
        body: {
          email: "",
          password: "",
        },
        errors: {},
      },
    };
  },
  methods: {
    errors(res) {
      this.form.errors = Object.fromEntries(
        res.data.errors.map((item) => [item.field, item.msg])
      );
    },
    registerDefault() {
      this.$auth
        .register({
          data: this.form.body, // Axios
          remember: this.form.remember ? '{"name": "Default"}' : null,
          fetchUser: this.form.fetchUser,
          autoLogin: this.form.autoLogin,
          staySignedIn: this.form.staySignedIn,
        })
        .then(
          (res) => {
            this.toast.info(res.data.message);
          },
          (res) => {
            this.errors(
              res.response || // Axios
                res // VueResource
            );
          }
        );
    },
    registerRedirect() {
      this.$auth
        .register({
          body: this.form.body, // VueResource
          data: this.form.body, // Axios
          redirect: { name: "user-account" },
          remember: this.form.remember ? '{"name": "Redirect"}' : null,
          fetchUser: this.form.fetchUser,
          autoLogin: this.form.autoLogin,
          staySignedIn: this.form.staySignedIn,
        })
        .then(null, (res) => {
          this.errors(
            res.response || // Axios
              res // VueResource
          );
        });
    },
    registerThen() {
      this.$auth
        .register({
          body: this.form.body, // VueResource
          data: this.form.body, // Axios
          fetchUser: this.form.fetchUser,
          autoLogin: this.form.autoLogin,
          staySignedIn: this.form.staySignedIn,
        })
        .then(
          () => {
            if (this.form.remember) {
              this.$auth.remember(
                JSON.stringify({
                  name: this.$auth.user().first_name,
                })
              );
            }
            this.$router.push({ name: "user-account" });
          },
          (res) => {
            this.errors(
              res.response || // Axios
                res // VueResource
            );
          }
        );
    },
    registerVuex() {
      this.$store
        .dispatch("auth/register", {
          body: this.form.body, // VueResource
          data: this.form.body, // Axios
          remember: this.form.remember,
          fetchUser: this.form.fetchUser,
          autoLogin: this.form.autoLogin,
          staySignedIn: this.form.staySignedIn,
        })
        .then(null, (res) => {
          this.errors(
            res.response || // Axios
              res // VueResource
          );
        });
    },
  },
};
</script>