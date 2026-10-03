#pragma once
#ifndef QUEUE_H
#define QUEUE_H

#include"deque.h"

template<class T, class Cont = Deque<T>>
class Queue {
public:
	using container_type = Cont;

	using value_type		= typename container_type::value_type;
	using size_type			= typename container_type::size_type;
	using reference			= typename container_type::reference;
	using const_reference	= typename container_type::const_reference;

	Queue() = default;

	explicit Queue(const container_type& cont)
		: _cont(cont) {}

	explicit Queue(container_type&& _cont)
		noexcept(std::is_nothrow_move_constructible_v<container_type>)
		: _cont(std::move(_cont)) {}

	Queue(const Queue& other)
		: _cont(other._cont) {}

	Queue(Queue&& other)
		noexcept(std::is_nothrow_move_constructible_v<container_type>)
		: _cont(std::move(other._cont)) {}

	Queue& operator=(const Queue& other) {
		if (this != std::addressof(other)) {
			_cont = other._cont;
		}
		return *this;
	}

	Queue& operator=(Queue&& other)
		noexcept(std::is_nothrow_move_assignable_v<container_type>)
	{
		if (this != std::addressof(other)) {
			_cont = std::move(other._cont);
		}
		return *this;
	}

	[[nodiscard]] bool is_empty() const {
		if constexpr (requires { _cont.is_empty(); }) {
			return _cont.is_empty();
		}
		else if constexpr (requires { _cont.empty(); }) {
			return _cont.empty();
		}
		else {
			return _cont.size() == 0;
		}
	}

	[[nodiscard]] size_type size() const noexcept(noexcept(_cont.size())) {
		return _cont.size();
	}

	[[nodiscard]] reference front() noexcept(noexcept(_cont.front())) {
		return _cont.front();
	}

	[[nodiscard]] const_reference front() const noexcept(noexcept(_cont.front())) {
		return _cont.front();
	}

	[[nodiscard]] reference back() noexcept(noexcept(_cont.back())) {
		return _cont.back();
	}

	[[nodiscard]] const_reference back() const noexcept(noexcept(_cont.back())) {
		return _cont.back();
	}

	template<class... Args>
	decltype(auto) emplace(Args&&... args) {
		// Insert by perfectly forwarding args at back
		return _cont.emplace_back(std::forward<Args>(args)...);
	}

	void push(const value_type& val) {
		// Insert by copying val at back
		_cont.push_back(val);
	}

	void push(value_type&& val) {
		// Insert by moving val at back
		_cont.push_back(std::move(val));
	}

	void pop() noexcept(noexcept(_cont.pop_front())) {
		// Remove element at front
		_cont.pop_front();
	}

	void swap(Queue& other) noexcept(std::is_nothrow_swappable_v<container_type>) {
		using std::swap;
		swap(_cont, other._cont);
	}

	[[nodiscard]] const container_type& container() const noexcept {
		return _cont;
	}

private:
	Cont _cont;
};

template<class T, class Cont>
	requires(std::is_swappable_v<Cont>)
void swap(Queue<T, Cont>& lhs, Queue<T, Cont>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Cont>
[[nodiscard]] bool operator==(const Queue<T, Cont>& lhs, const Queue<T, Cont>& rhs) {
	return lhs.container() == rhs.container();
}

template<class T, std::three_way_comparable Cont>
[[nodiscard]] compare::SynthThreeWayCompareResult<Cont> operator<=>(
	const Queue<T, Cont>& lhs, const Queue<T, Cont>& rhs
) {
	return lhs.container() <=> rhs.container();
}
#endif // QUEUE_H