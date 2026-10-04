#pragma once
#ifndef QUEUE_H
#define QUEUE_H

#include"deque.h"
#include"dynamic_array.h"

#include"algorithm.hpp"

/*
Possible underlying containers for queue:
	- Deque
	- List
	- ... or any other container that supports O(1) insertion and deletion at both ends
*/
template<class T, class Cont = Deque<T>>
class Queue {
public:
	using container_type	= Cont;

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

	Queue(const Queue&) = default;
	Queue(Queue&&)		= default;

	Queue& operator=(const Queue&)	= default;
	Queue& operator=(Queue&&)		= default;

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
	Cont _cont{};
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

template<class Cont>
Queue(Cont) -> Queue<typename Cont::value_type, Cont>;

/*
Possible underlying containers for priority queue:
	- Deque
	- DynamicArray
	- ... or any other container that supports random access
*/
template<class T, class Cont = DynamicArray<T>, class Comp = std::less<typename Cont::value_type>>
class PriorityQueue {
public:
	using container_type	= Cont;
	using value_compare		= Comp;

	using value_type		= typename container_type::value_type;
	using size_type			= typename container_type::size_type;
	using reference			= typename container_type::reference;
	using const_reference	= typename container_type::const_reference;

public:
	PriorityQueue() = default;

	explicit PriorityQueue(const Comp& comp)
		noexcept(std::is_nothrow_default_constructible_v<container_type> &&
				 std::is_nothrow_copy_constructible_v<value_compare>)
		: _cont(), _comp(comp) {
	}

	PriorityQueue(const Cont& cont, const Comp& comp)
		: _cont(cont), _comp(comp) {
		this->_make_heap();
	}

	PriorityQueue(Cont&& cont, const Comp& comp)
		: _cont(std::move(cont)), _comp(comp) {
		this->_make_heap();
	}

	/*
	STL containers only support same-type iterators.
	We sacrifice some flexibility with sentinels to adapt STL containers, without having to write complicated helpers.
	*/
	template<std::input_iterator It>
	PriorityQueue(It first, It last)
		: _cont(std::move(first), std::move(last)), _comp() {
		this->_make_heap();
	}

	template<std::input_iterator It>
	PriorityQueue(It first, It last, const Comp& comp)
		: _cont(std::move(first), std::move(last)), _comp(comp) {
		this->_make_heap();
	}

	template<std::input_iterator It>
	PriorityQueue(It first, It last, const Cont& cont, const Comp& comp)
		: _cont(cont), _comp(comp) {
		_cont.insert(_cont.end(), std::move(first), std::move(last));
		this->_make_heap();
	}

	template<std::input_iterator It>
	PriorityQueue(It first, It last, Cont&& cont, const Comp& comp)
		: _cont(std::move(cont)), _comp(comp) {
		_cont.insert(_cont.end(), std::move(first), std::move(last));
		this->_make_heap();
	}

	PriorityQueue(const PriorityQueue&) = default;
	PriorityQueue(PriorityQueue&&)		= default;

	PriorityQueue& operator=(const PriorityQueue&)	= default;
	PriorityQueue& operator=(PriorityQueue&&)		= default;

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

	// Top element is read-only in priority queue
	[[nodiscard]] const_reference top() const noexcept(noexcept(_cont.front())) {
		return _cont.front();
	}

	template<class... Args>
	void emplace(Args&&... args) {
		// Insert by perfectly forwarding args at top, then reheap
		// Insert at the end of the container
		_cont.emplace_back(std::forward<Args>(args)...);
		// Move the inserted element into correct position, maintaining heap property
		heap::push_heap(_cont.begin(), _cont.end(), _comp);
	}

	void push(const value_type& val) {
		// Insert by copying val at top, then reheap
		_cont.push_back(val);
		heap::push_heap(_cont.begin(), _cont.end(), _comp);
	}

	void push(value_type&& val) {
		// Insert by moving val at top, then reheap
		_cont.push_back(std::move(val));
		heap::push_heap(_cont.begin(), _cont.end(), _comp);
	}

	void pop() {
		// Remove element at top, then reheap
		// Move the top element to the end of the container, maintaining heap property
		heap::pop_heap(_cont.begin(), _cont.end(), _comp);
		// Actually remove it from the container
		_cont.pop_back();
	}

	void swap(PriorityQueue& other)
		noexcept(std::is_nothrow_swappable_v<container_type> &&
				 std::is_nothrow_swappable_v<value_compare>)
	{
		using std::swap;
		swap(_cont, other._cont);
		swap(_comp, other._comp);
	}

private:
	void _make_heap() {
		heap::make_heap(_cont.begin(), _cont.end(), _comp);
	}

private:
	Cont _cont{};
	Comp _comp{};
};

template<class T, class Cont, class Comp>
	requires(std::is_swappable_v<Cont>)
void swap(PriorityQueue<T, Cont, Comp>& lhs, PriorityQueue<T, Cont, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

/*
Priority queue does not support comparison operators because identical sets of elements can form
multiple valid binary heap layouts in the underlying container.
In this context, standard lexicographical comparisons become semantically meaningless and unreliable.
*/

template<class Cont>
Queue(Cont) -> Queue<typename Cont::value_type, Cont>;

template<class Cont, class Comp>
PriorityQueue(Cont, Comp) -> PriorityQueue<typename Cont::value_type, Cont, Comp>;

template<std::input_iterator It>
PriorityQueue(It, It) -> PriorityQueue<std::iter_value_t<It>>;

template<std::input_iterator It, class Comp>
PriorityQueue(It, It, Comp) -> PriorityQueue<std::iter_value_t<It>, DynamicArray<std::iter_value_t<It>>, Comp>;

template<std::input_iterator It, class Cont, class Comp>
PriorityQueue(It, It, Cont, Comp) -> PriorityQueue<std::iter_value_t<It>, Cont, Comp>;
#endif // QUEUE_H