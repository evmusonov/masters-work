const authRoutes = ['user-page'];
const unauthRoutes = ['login', 'register'];

function isAuth() {
	if (localStorage.getItem('access_token')) {
		return true;
	}

	return false;
}

export {authRoutes, unauthRoutes, isAuth};