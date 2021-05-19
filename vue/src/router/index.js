import { createWebHistory, createRouter } from "vue-router";
import Home from "./../components/Home.vue";
import Register from "./../components/user/Register.vue";
import Login from "./../components/user/Login.vue";
import UserPage from "./../components/user/UserPage.vue";
import UserSettings from "./../components/user/UserSettings.vue";
import NotFound from "./../components/NotFound.vue";
import Admin from "./../components/admin/Admin.vue";
import Editor from "./../components/Editor.vue";
import CoursePage from "./../components/course/Page.vue";
import StudyPage from "./../components/course/Study.vue";
import CourseTest from "./../components/course/Test.vue";
import UserProgress from "./../components/user/Progress.vue";

import { authRoutes, unauthRoutes } from "./../modules/auth";
import store from './../modules/store'

const routes = [
  {
    path: "/",
    name: "home",
    component: Home,
  },
  {
    path: "/register",
    name: "register",
    component: Register,
  },
  {
    path: "/login",
    name: "login",
    component: Login,
  },
  {
    path: "/user",
    name: "user-page",
    component: UserPage,
  },
  {
    path: "/user/settings",
    name: "user-settings",
    component: UserSettings,
  },
  {
    path: "/user/courses/:uid",
    name: "user-progress",
    component: UserProgress,
  },
  {
    path: "/admin",
    name: "admin-main",
    component: Admin,
    children: [
      {
        path: "users",
        name: "admin-users",
        component: Admin,
        children: [
          {
            path: ":uid/edit",
            name: "admin-users-edit",
            component: Admin
          },
        ]
      },
      {
        path: "courses",
        name: "admin-courses",
        component: Admin,
        children: [
          {
            path: "add",
            name: "admin-courses-add",
            component: Admin
          },
          {
            path: ":uid/edit",
            name: "admin-courses-edit",
            component: Admin
          },
        ]
      },
      {
        path: "ontos",
        name: "admin-ontos",
        component: Admin,
        children: [
          {
            path: "add",
            name: "admin-ontos-add",
            component: Admin
          },
          {
            path: ":uid/edit",
            name: "admin-ontos-edit",
            component: Admin
          },
        ]
      },
    ]
  },
  {
    path: "/courses/:uid",
    name: "courses-page",
    component: CoursePage
  },
  {
    path: "/courses/study/:uid",
    name: "courses-study-page",
    component: StudyPage
  },
  {
    path: "/courses/:course/tests/:test",
    name: "courses-test-page",
    component: CourseTest
  },
  {
    path: "/editor",
    name: "editor",
    component: Editor,
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
    next({ name: 'login' })
  } else if (unauthRoutes.includes(to.name) == true && store.getters.isAuth) {
    next({ name: 'user-page' })
  } else {
    next()
  }
})

export default router;