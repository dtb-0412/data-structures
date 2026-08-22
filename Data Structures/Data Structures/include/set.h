#pragma once
#ifndef SET_H
#define SET_H

#include"avl_tree.h"
#include"concepts.hpp"

template<class T, class Comp, class Cont>
class _Set {
private:
	using _BaseCont = Cont;

public:
	using key_type		= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseCont::value_compare;

	using value_type		= typename _BaseCont::value_type;
	using size_type			= typename _BaseCont::size_type;
	using difference_type	= typename _BaseCont::difference_type;
	using pointer			= typename _BaseCont::pointer;
	using const_pointer		= typename _BaseCont::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseCont::iterator;
	using const_iterator	= typename _BaseCont::const_iterator;

	using reverse_iterator			= typename _BaseCont::reverse_iterator;
	using const_reverse_iterator	= typename _BaseCont::const_reverse_iterator;

	using node_handle			= typename _BaseCont::node_handle;
	using insert_return_type	= _InsertReturnType<iterator, node_handle>;

public:
	_Set()
		: _cont(key_compare()) {}

	explicit _Set(const key_compare& comp)
		: _cont(comp) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	_Set(It first, Se last)
		: _cont(first, last, key_compare()) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	_Set(It first, Se last, const key_compare& comp)
		: _cont(first, last, comp) {}

	_Set(const _Set& other)
		: _cont(other._cont) {}

	_Set(_Set&& other) noexcept(std::is_nothrow_move_constructible_v<Cont>)
		: _cont(std::move(other._cont)) {}

	_Set(std::initializer_list<value_type> initList)
		: _cont(initList, key_compare()) {}

	_Set(std::initializer_list<value_type> initList, const key_compare& comp)
		: _cont(initList, comp) {}

	_Set& operator=(const _Set& other) {
		_cont = other._cont;
		return *this;
	}

	_Set& operator=(_Set&& other) noexcept(std::is_nothrow_move_assignable_v<Cont>) {
		_cont = std::move(other._cont);
		return *this;
	}

	_Set& operator=(std::initializer_list<value_type> initList) {
		_cont = initList;
		return *this;
	}

	[[nodiscard]] iterator begin() noexcept {
		return _cont.begin();
	}

	[[nodiscard]] const_iterator begin() const noexcept {
		return _cont.begin();
	}

	[[nodiscard]] iterator end() noexcept {
		return _cont.end();
	}

	[[nodiscard]] const_iterator end() const noexcept {
		return _cont.end();
	}

	[[nodiscard]] const_iterator cbegin() const noexcept {
		return _cont.cbegin();
	}

	[[nodiscard]] const_iterator cend() const noexcept {
		return _cont.cend();
	}

	[[nodiscard]] reverse_iterator rbegin() noexcept {
		return _cont.rbegin();
	}

	[[nodiscard]] const_reverse_iterator rbegin() const noexcept {
		return _cont.rbegin();
	}

	[[nodiscard]] reverse_iterator rend() noexcept {
		return _cont.rend();
	}

	[[nodiscard]] const_reverse_iterator rend() const noexcept {
		return _cont.rend();
	}

	[[nodiscard]] bool is_mpty() const noexcept {
		return _cont.is_empty();
	}

	[[nodiscard]] size_type size() const noexcept {
		return _cont.size();
	}

	[[nodiscard]] size_type max_size() const noexcept {
		return _cont.max_size();
	}

	[[nodiscard]] key_compare key_comp() const {
		return _cont.key_comp();
	}

	[[nodiscard]] value_compare value_comp() const {
		return _cont.value_comp();
	}

	template<class... Args>
	std::pair<iterator, bool> emplace(Args&&... args) {
		return _cont.emplace(std::forward<Args>(args)...);
	}

	template<class... Args>
	iterator emplace_hint(const_iterator hint, Args&&... args) {
		return _cont.emplace_hint(hint, std::forward<Args>(args)...);
	}

	std::pair<iterator, bool> insert(const value_type& value) {
		return _cont.insert(value);
	}

	std::pair<iterator, bool> insert(value_type&& value) {
		return _cont.insert(std::move(value));
	}

	iterator insert(const_iterator hint, const value_type& value) {
		return _cont.insert(hint, value);
	}

	iterator insert(const_iterator hint, value_type&& value) {
		return _cont.insert(hint, std::move(value));
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	void insert(It first, Se last) {
		_cont.insert(first, last);
	}

	void insert(std::initializer_list<value_type> ilist) {
		_cont.insert(ilist);
	}

	iterator erase(const_iterator pos) {
		return _cont.erase(pos);
	}

	iterator erase(const_iterator first, const_iterator last) {
		return _cont.erase(first, last);
	}

	size_type erase(const key_type& key) {
		return _cont.erase(key);
	}
	
	template<class KeyT>
		requires requires {
		typename key_compare::is_transparent;
		requires !std::convertible_to<KeyT, const_iterator>;
		requires !std::convertible_to<KeyT, iterator>;
	}
	size_type erase(KeyT&& key) {
		return _cont.erase(std::forward<KeyT>(key));
	}

	void clear() noexcept {
		_cont.clear();
	}

	void swap(_Set& other) noexcept(noexcept(_BaseCont::swap(other._cont))) {
		using std::swap;
		swap(_cont, other._cont);
	}

	[[nodiscard]] iterator find(const key_type& key) {
		return _cont.find(key);
	}

	[[nodiscard]] const_iterator find(const key_type& key) const {
		return _cont.find(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] iterator find(const KeyT& key) {
		return _cont.find(key);
	}

	[[nodiscard]] size_type count(const key_type& key) const {
		return _cont.count(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] size_type count(const KeyT& key) const {
		return _cont.count(key);
	}

	[[nodiscard]] bool contains(const key_type& key) const {
		return _cont.contains(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] bool contains(const KeyT& key) const {
		return _cont.contains(key);
	}

	[[nodiscard]] iterator lower_bound(const key_type& key) {
		return _cont.lower_bound(key);
	}

	[[nodiscard]] const_iterator lower_bound(const key_type& key) const {
		return _cont.lower_bound(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] iterator lower_bound(const KeyT& key) {
		return _cont.lower_bound(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] const_iterator lower_bound(const KeyT& key) const {
		return _cont.lower_bound(key);
	}

	[[nodiscard]] iterator upper_bound(const key_type& key) {
		return _cont.upper_bound(key);
	}

	[[nodiscard]] const_iterator upper_bound(const key_type& key) const {
		return _cont.upper_bound(key);
	}
	
	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] iterator upper_bound(const KeyT& key) {
		return _cont.upper_bound(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] const_iterator upper_bound(const KeyT& key) const {
		return _cont.upper_bound(key);
	}

	[[nodiscard]] std::pair<iterator, iterator> equal_range(const key_type& key) {
		return _cont.equal_range(key);
	}

	[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const {
		return _cont.equal_range(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] std::pair<iterator, iterator> equal_range(const KeyT& key) {
		return _cont.equal_range(key);
	}

	template<class KeyT>
		requires requires { typename key_compare::is_transparent; }
	[[nodiscard]] std::pair<const_iterator, const_iterator> equal_range(const KeyT& key) const {
		return _cont.equal_range(key);
	}

	template<class OtherCont>
	void merge(_Set<T, Comp, OtherCont>& other) {
		_cont.merge(other._cont);
	}

	insert_return_type insert(node_handle&& handle) {
		return _cont.insert(std::move(handle));
	}

	iterator insert(const_iterator hint, node_handle&& handle) {
		return _cont.insert(hint, std::move(handle));
	}

	node_handle extract(const_iterator pos) {
		return _cont.extract(pos);
	}

	node_handle extract(const key_type& key) {
		return _cont.extract(key);
	}

private:
	_BaseCont _cont;
};

template<class T, class Comp, class Cont>
void swap(_Set<T, Comp, Cont>& lhs, _Set<T, Comp, Cont>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp = std::less<>, class Cont = AVLTree<T, Comp>>
using Set = _Set<T, Comp, Cont>;

template<class T, class Comp = std::less<>, class Cont = AVLMultiTree<T, Comp>>
using MultiSet = _Set<T, Comp, Cont>;
#endif // SET_H