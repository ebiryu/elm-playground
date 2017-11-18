var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber')
var browserSync = require('browser-sync').create();

var paths = {
	dist    : "dist",
	server  : 'server',
	copy    : ['src/index.html', 'src/**/*.js', '*.css', 'src/*.csv'],
	elm     : "src/**/*.elm",
	elmMain : "src/Main.elm"
};

gulp.task('copy', function() {
	return gulp.src(paths.copy)
  	.pipe(gulp.dest(paths.dist));
});

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  return gulp.src(paths.elm)
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest('dist/'));
});

gulp.task('default',['watch']);

gulp.task('elm-bundle', ['elm-init'], function(){
  return gulp.src(paths.elmMain)
    .pipe(plumber())
    .pipe(elm.bundle('app.js',{"debug": true}))
    .pipe(gulp.dest('dist/'));
});

gulp.task('watch', function() {
  browserSync.init({
		server: {
			baseDir: "./dist",
			routes: {
				"/bower_components": "bower_components",
				"/node_modules": "node_modules"
			}
		}
	});
	console.log("Listening on port 3000");

	gulp.watch(paths.copy, ['copy']);
	gulp.watch(paths.elm, ['elm-bundle']);
	gulp.watch(paths.dist+"/*.{js,html}").on('change', browserSync.reload);
	gulp.watch(paths.dist+"/*.{css}").on('change', browserSync.stream);
});
