const authRoutes = ['UserPage'];
const unauthRoutes = ['Login', 'Register'];

function isAuth() {
	if (localStorage.getItem('access_token')) {
		return true;
	}

	return false;
}

export {authRoutes, unauthRoutes, isAuth};