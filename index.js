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

function getRandomArbitrary(min, max) {
	return Math.random() * (max - min) + min;
}

const cardWidth = document.querySelector('.personcard').offsetWidth;
const cardHeight = document.querySelector('.personcard').offsetHeight;
const maxWidth = document.documentElement.clientWidth - cardWidth;
const maxHeight = document.documentElement.clientHeight - cardHeight;

const state = Array.prototype.map.call(document.getElementsByClassName('personcard'), e => {
	return {
		dom: e,
		x: getRandomArbitrary(cardWidth, maxWidth),
		dx: getRandomArbitrary(0.75, 1.5),
		y: getRandomArbitrary(cardHeight, maxHeight),
		dy: getRandomArbitrary(0.75, 1.5),
	};
}
);

{
	const cb = () => {
		for (el of state) {
			// touch another card
			for (el2 of state.filter(x => x != el)) {
				if (distance(el, el2) < 50) { bounce(el); break; }
			}

			// screen borders
			if (el.x < 0 || 
			    el.x > maxWidth) {
				el.dx = -el.dx;
			}
			if (el.y < 0 || 
			    el.y > maxHeight) {
				el.dy = -el.dy;
			}

			el.dom.style.left = el.x + 'px';
			el.dom.style.top = el.y + 'px';
			el.x += el.dx;
			el.y += el.dy;
		}
		window.requestAnimationFrame(cb);
	};
	window.requestAnimationFrame(cb);
}
