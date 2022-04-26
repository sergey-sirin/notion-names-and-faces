function popup(name, bio) {
	alert(name + '\n\n' + bio);
}

function distance(el1, el2) {
	return Math.sqrt((el2.x - el1.x) ** 2 + (el2.y - el1.y) ** 2);
}

function bounce(el) {
	el.dx = -el.dx;
	el.dy = -el.dy;
}

const state = Array.prototype.map.call(document.getElementsByClassName('personcard'), e => {
	return {
		dom: e,
		x: Math.random() * 100, // XXX
		dx: Math.random() * 10,
		y: Math.random() * 100, // XXX
		dy: Math.random() * 10, 
	};
}
);

{
	const cb = () => {
		for (el of state) {
			el.dom.style.left = el.x + 'px';
			el.dom.style.top = el.y + 'px';
			el.x += el.dx;
			el.y += el.dy;

			for (el2 of state.filter(x => x != el)) {
				if (distance(el, el2) < 50) { bounce(el); }
			}

			// screen borders
			if (el.x < 0 || 
			    el.x > document.documentElement.clientWidth - document.querySelector('.personcard').offsetWidth) {
				el.dx = -el.dx;
			}
			if (el.y < 0 || 
			    el.y > document.documentElement.clientHeight - document.querySelector('.personcard').offsetHeight) {
				el.dy = -el.dy;
			}
		}
		window.requestAnimationFrame(cb);
	};
	window.requestAnimationFrame(cb);
}
