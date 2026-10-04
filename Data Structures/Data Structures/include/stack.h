#pragma once
#ifndef STACK_H
#define STACK_H

#include"deque.h"

/*
Possible underlying containers for stack:
	- Deque
	- DynamicArray
	- List
	- ... or any other container that supports O(1) insertion and deletion at back
*/
template<class T, class Cont = Deque<T>>
class Stack {
public:
	using container_type	= Cont;

	using value_type		= typename container_type::value_type;
	using size_type			= typename container_type::size_type;
	using reference			= typename container_type::reference;
	using const_reference	= typename container_type::const_reference;

	Stack() = default;

	explicit Stack(const container_type& cont)
		: _cont(cont) {}

	explicit Stack(container_type&& _cont)
		noexcept(std::is_nothrow_move_constructible_v<container_type>)
		: _cont(std::move(_cont)) {}

	Stack(const Stack&) = default;
	Stack(Stack&&)		= default;

	Stack& operator=(const Stack&)	= default;
	Stack& operator=(Stack&&)		= default;

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

	[[nodiscard]] reference top() noexcept(noexcept(_cont.back())) {
		return _cont.back();
	}

	[[nodiscard]] const_reference top() const noexcept(noexcept(_cont.back())) {
		return _cont.back();
	}

	template<class... Args>
	decltype(auto) emplace(Args&&... args) {
		// Insert by perfectly forwarding args at top
		return _cont.emplace_back(std::forward<Args>(args)...);
	}

	void push(const value_type& val) {
		// Insert by copying val at top
		_cont.push_back(val);
	}

	void push(value_type&& val) {
		// Insert by moving val at top
		_cont.push_back(std::move(val));
	}

	void pop() noexcept(noexcept(_cont.pop_back())) {
		// Remove element at top
		_cont.pop_back();
	}

	void swap(Stack& other) noexcept(std::is_nothrow_swappable_v<container_type>) {
		using std::swap;
		swap(_cont, other._cont);
	}

	[[nodiscard]] const container_type& container() const noexcept {
		return _cont;
	}

private:
	Cont _cont{}; // Value-initialize here, since we use default empty constructor for convenience
};

template<class T, class Cont>
	requires(std::is_swappable_v<Cont>)
void swap(Stack<T, Cont>& lhs, Stack<T, Cont>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Cont>
[[nodiscard]] bool operator==(const Stack<T, Cont>& lhs, const Stack<T, Cont>& rhs) {
	return lhs.container() == rhs.container();
}

template<class T, std::three_way_comparable Cont>
[[nodiscard]] compare::SynthThreeWayCompareResult<Cont> operator<=>(
	const Stack<T, Cont>& lhs, const Stack<T, Cont>& rhs
) {
	return lhs.container() <=> rhs.container();
}

// Deduction guide (CTAD)
template<class Cont>
Stack(Cont) -> Stack<typename Cont::value_type, Cont>;
#endif // STACK_H