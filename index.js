const cardWidth = document.querySelector('.personcard').offsetWidth;
const cardHeight = document.querySelector('.personcard').offsetHeight;
const maxWidth = document.documentElement.clientWidth - cardWidth;
const maxHeight = document.documentElement.clientHeight - cardHeight;

function popup(name, bio) {
	alert(name + '\n\n' + bio);
}

function centroid(el) {
	return [el.x + cardWidth / 2, el.y + cardHeight / 2];
}

function distance(el1, el2) {
	const [[el1x, el1y], [el2x, el2y]] = [centroid(el1), centroid(el2)];
	return Math.sqrt((el2x - el1x) ** 2 + (el2y - el1y) ** 2);
}

function bounce(el) {
	el.dx = -el.dx;
	el.dy = -el.dy;
}

function getRandomArbitrary(min, max) {
	return Math.random() * (max - min) + min;
}

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
				if (distance(el, el2) < cardWidth) { bounce(el); break; }
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
