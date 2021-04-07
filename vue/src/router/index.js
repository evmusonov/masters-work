import { createWebHistory, createRouter } from "vue-router";
import Home from "./../components/Home.vue";
import Register from "./../components/Register.vue";
import Login from "./../components/Login.vue";
import UserPage from "./../components/UserPage.vue";
import UserSettings from "./../components/UserSettings.vue";
import NotFound from "./../components/NotFound.vue";
import { authRoutes, unauthRoutes } from "./../modules/auth";
import store from './../modules/store'

const routes = [
  {
    path: "/",
    name: "Home",
    component: Home,
  },
  {
    path: "/register",
    name: "Register",
    component: Register,
  },
  {
    path: "/login",
    name: "Login",
    component: Login,
  },
  {
    path: "/user",
    name: "UserPage",
    component: UserPage,
  },
  {
    path: "/user/settings",
    name: "UserSettings",
    component: UserSettings,
  },
  {
    path: "/:catchAll(.*)",
    component: NotFound,
  },
];

const router = createRouter({
  history: createWebHistory(),
  routes,
});

router.beforeEach((to, from, next) => {
  if (authRoutes.includes(to.name) == true && !store.getters.isAuth) {
    next({ name: 'Login' })
  } else if (unauthRoutes.includes(to.name) == true && store.getters.isAuth) {
    next({ name: 'UserPage' })
  } else {
    next()
  }
})

export default router;