require(['jquery'], function ($) {
	console.log($('.reg-title').text());
	/* GUEST BLOCK (auth/registration) */
	$(document).ready(function() {
		$('.register-form').hide();
		$('.auth-title').css({"color": "black", "text-decoration": "none"});
		
		$('.auth-title').on('click', function() {
			$('.auth-form').show();
			$('.register-form').hide();
			$('.reg-title').css({"color": "blue", "text-decoration": "underline"});
			$('.auth-title').css({"color": "black", "text-decoration": "none"});
		});
		
		$('.reg-title').on('click', function() {
			$('.auth-form').hide();
			$('.register-form').show();
			$('.auth-title').css({"color": "blue", "text-decoration": "underline"});
			$('.reg-title').css({"color": "black", "text-decoration": "none"});
		});
	});
	/* END GUEST BLOCK (auth/registration) */
});