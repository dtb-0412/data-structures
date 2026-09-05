#pragma once
#ifndef MAP_H
#define MAP_H

#include"avl_tree.h"

template<class DerivedT, class T1, class T2>
struct _NodeHandleMapBase {
	using key_type		= T1;
	using mapped_type	= T2;

	key_type& key() const noexcept {
		return this->_get_mutable_pair().first;
	}

	mapped_type& mapped() const noexcept {
		return this->_get_mutable_pair().second;
	}

private:
	/*
	C++17 standards require that when used as internal structure for map, tree node stores a key-value pair,
	with key being immutable. This is to prevent users from modifying node's key, compromising the tree's order.
	However, the standards also require node's key to be mutable after it is extracted from tree, when users can
	safely modify it.

	To enable this, we use reinterpret_cast to forcefully cast pair<const key, mapped>& to pair<key, mapped>&.

	Removing constness using const_cast or reinterpret_cast to overwrite an object that is initially const is
	considered UB by the standards.
	On MSVC compiler, reinterpret_cast between 2 layouts with the same size/alignment is practically safe.
	*/
	using _MutablePair = std::pair<key_type, mapped_type>;

	_MutablePair& _get_mutable_pair() const {
		const auto& self = static_cast<const DerivedT&>(*this);
		auto& data = self.get_pointer()->value;
		return reinterpret_cast<_MutablePair&>(data);
	}
};

template<class KeyT, class... Args>
struct _InPlaceMapKeyExtractorBase {
	static constexpr bool isExtractable = false;
};

template<class KeyT, class T>
struct _InPlaceMapKeyExtractorBase<KeyT, KeyT, T> {
	// If we would call the emplace family with (key, value), we can use the first parameter as the key
	static constexpr bool isExtractable = true;

	static const KeyT& extract(const KeyT& key, const T&) noexcept {
		return key;
	}
};

template<class KeyT, class First, class Second>
struct _InPlaceMapKeyExtractorBase<KeyT, std::pair<First, Second>> {
	// If we would call the emplace family with std::pair<first, second>, we can use the first parameter as the key
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<First>>;

	static const KeyT& extract(const std::pair<First, Second>& val) noexcept {
		return val.first;
	}
};

template<class KeyT, class First, class... Args>
struct _InPlaceMapKeyExtractorBase<KeyT, std::piecewise_construct_t, std::tuple<First>, std::tuple<Args...>> {
	/*
	If we would call the emplace family with std::piecewise_construct_t, we can use std::get<0>() on the first tuple as the key
	This is a very niche case, used only when:
		- Mapped type is non-copyable and non-movable (we cannot create temporary object, then copy/move into tuple)
		- Mapped type must be constructed in place at node's memory (std::forward_as_tuple() must be used here)
	*/
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<First>>;

	static const KeyT& extract(
		const std::piecewise_construct_t&, const std::tuple<First>& key, const std::tuple<Args...>&
	) noexcept {
		return std::get<0>(key);
	}
};

/*
From C++23, std::pair can be constructed from tuple-like objects, specifically std::tuple<U, V> and std::array<T, 2>.
If we would call the emplace family with std::tuple<U, V> or std::array<T, 2>, we can use std::get<0> or operator[](0)
to get the first element as key.
*/
#if _MSVC_LANG > 202002L
template<class KeyT, class T>
struct _InPlaceMapKeyExtractorBase<KeyT, std::array<T, 2>> {
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<T>>;

	static const KeyT& extract(const std::array<T, 2>& val) noexcept {
		return val[0];
	}
};

template<class KeyT, class First, class Second>
struct _InPlaceMapKeyExtractorBase<KeyT, std::tuple<First, Second>> {
	static constexpr bool isExtractable = std::is_same_v<KeyT, std::remove_cvref_t<First>>;

	static const KeyT& extract(const std::tuple<First, Second>& val) noexcept {
		return std::get<0>(val);
	}
};
#endif // From C++23

template<class... Args>
using _InPlaceMapKeyExtractor = _InPlaceMapKeyExtractorBase<std::remove_cvref_t<Args>...>;

template<class T1, class T2, class Comp, template<class> class NodeT, bool _isMulti>
class _BSTreeMapTraits {
public:
	using key_type		= T1;
	using value_type	= std::pair<const key_type, T2>;
	using key_compare	= Comp;

	using node_type		= NodeT<value_type>;
	using node_pointer	= typename node_type::node_pointer;

	using node_handle	= _NodeHandle<_NodeHandleMapBase, node_type, key_type, T2>;

	class value_compare {
		[[nodiscard]] bool operator()(const value_type& lhs, const value_type& rhs) const {
			// Compare nodes by comparing map key
			return comp(lhs.first, rhs.first);
		}

	protected:
		template<class, template<class...> class>
		friend class _BSTree;

		value_compare(key_compare comp) // Prevent creation from user
			: comp(comp) {}

		key_compare comp;
	};

	static constexpr bool isMulti	= _isMulti;
	static constexpr bool isMap		= true;

	template<class... Args>
	using in_place_key_extractor = _InPlaceMapKeyExtractor<key_type, Args...>;

	template<class T1, class T2>
	static const key_type& key_from_node(const std::pair<T1, T2>& val) {
		return val.first;
	}
};

template<class T1, class T2, class Comp = std::less<T1>>
class Map : public _BSTree<
	_BSTreeMapTraits<T1, T2, Comp, _AVLTreeNode, false>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeMapTraits<T1, T2, Comp, _AVLTreeNode, false>, _AVLTreeCore>;

	using _NodeType		= typename _BaseTree::_NodeType;
	using _NodePointer	= typename _BaseTree::_NodePointer;

public:
	using key_type		= T1;
	using mapped_type	= T2;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= typename _BaseTree::value_type;
	using size_type			= typename _BaseTree::size_type;
	using difference_type	= typename _BaseTree::difference_type;
	using pointer			= typename _BaseTree::pointer;
	using const_pointer		= typename _BaseTree::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseTree::iterator;
	using const_iterator	= typename _BaseTree::const_iterator;

	using reverse_iterator			= typename _BaseTree::reverse_iterator;
	using const_reverse_iterator	= typename _BaseTree::const_reverse_iterator;

	using node_handle			= typename _BaseTree::node_handle;
	using insert_return_type	= typename _BaseTree::insert_return_type;

	using _BaseTree::_BaseTree;

	mapped_type& operator[](const key_type& key) {
		// Insert key-value pair if key is absent (value is value-initialized), otherwise return reference to the value mapped by key
		return this->_try_emplace(key).first->value.second;
	}

	[[nodiscard]] mapped_type& at(const key_type& key) {
		// Return reference to the value mapped by key
		const auto result = this->_find_lower_bound(key);
		if (!this->_is_lower_bound_duplicate(result.bound, key)) {
			this->_subscription_error();
		}
		return result.bound->value.second;
	}

	[[nodiscard]] const mapped_type& at(const key_type& key) const {
		// Return reference to the value mapped by key
		const auto result = this->_find_lower_bound(key);
		if (!this->_is_lower_bound_duplicate(result.bound, key)) {
			this->_subscription_error();
		}
		return result.bound->value.second;
	}

	template<class... Args>
	std::pair<iterator, bool> try_emplace(const key_type& key, Args&&... args) {
		// Insert by constructing in place using args and copying from key, does not construct whole node unless key is absent
		const auto result = this->_try_emplace(key, std::forward<Args>(args)...);
		return { iterator(result.first), result.second };
	}

	template<class... Args>
	iterator try_emplace_hint(const_iterator hint, const key_type& key, Args&&... args) {
		// Insert with hint by constructing in place using args and copying from key, does not construct whole node unless key is absent
		const auto result = this->_try_emplace_hint(hint.ptr, key, std::forward<Args>(args)...);
		return iterator(result);
	}

	template<class... Args>
	std::pair<iterator, bool> try_emplace(key_type&& key, Args&&... args) {
		// Insert by constructing in place using args and moving from key, does not construct whole node unless key is absent
		const auto result = this->_try_emplace(key, std::forward<Args>(args)...);
		return { iterator(result.first), result.second };
	}

	template<class... Args>
	iterator try_emplace_hint(const_iterator hint, key_type&& key, Args&&... args) {
		// Insert with hint by constructing in place using args and moving from key, does not construct whole node unless key is absent
		const auto result = this->_try_emplace_hint(hint.ptr, key, std::forward<Args>(args)...);
		return iterator(result);
	}

	template<class MappedT>
	std::pair<iterator, bool> insert_or_assign(const key_type& key, MappedT&& mapped) {
		// Insert key-value pair if key is absent, otherwise assign mapped to the value mapped by key
		const auto result = this->_insert_or_assign(key, std::forward<MappedT>(mapped));
		return { iterator(result.first), result.second };
	}

	template<class MappedT>
	iterator insert_or_assign_hint(const_iterator hint, const key_type& key, MappedT&& mapped) {
		// Insert key-value pair if key is absent, otherwise assign mapped to the value mapped by key
		const auto result = this->_insert_or_assign_hint(hint.ptr, key, std::forward<MappedT>(mapped));
		return iterator(result);
	}

	template<class MappedT>
	std::pair<iterator, bool> insert_or_assign(key_type&& key, MappedT&& mapped) {
		// Insert key-value pair if key is absent, otherwise assign mapped to the value mapped by key
		const auto result = this->_insert_or_assign(key, std::forward<MappedT>(mapped));
		return { iterator(result.first), result.second };
	}

	template<class MappedT>
	iterator insert_or_assign_hint(const_iterator hint, key_type&& key, MappedT&& mapped) {
		// Insert key-value pair if key is absent, otherwise assign mapped to the value mapped by key
		const auto result = this->_insert_or_assign_hint(hint.ptr, key, std::forward<MappedT>(mapped));
		return iterator(result);
	}

	void swap(Map& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}

private:
	template<class KeyT, class... Args>
	std::pair<_NodePointer, bool> _try_emplace(KeyT&& key, Args&&... args) {
		// Insert by constructing in place using args, does not construct whole node unless key is absent
		const auto result = _BaseTree::_find_lower_bound(key);
		if (_BaseTree::_is_lower_bound_duplicate(result.bound, key)) {
			return { result.bound, false };
		}

		_BaseTree::_check_max_size();

		auto& data = this->_data;
		const _NodePointer newNode = _BSTreeTempNodeGuard<_NodeType>(
			data.head,
			std::piecewise_construct,
			std::forward_as_tuple(std::forward<KeyT>(key)),
			std::forward_as_tuple(std::forward<Args>(args)...)
		).release();

		return { data.insert(result.location, newNode), true };
	}

	template<class KeyT, class... Args>
	_NodePointer _try_emplace_hint(_NodePointer hintNode, KeyT&& key, Args&&... args) {
		// Insert with hint by constructing in place using args, does not construct whole node unless key is absent
		const auto result = _BaseTree::_find_hint(hintNode, key);
		if (result.isDuplicate) {
			return result.location.parent;
		}

		_BaseTree::_check_max_size();

		auto& data = this->_data;
		const _NodePointer newNode = _BSTreeTempNodeGuard<_NodeType>(
			data.head,
			std::piecewise_construct,
			std::forward_as_tuple(std::forward<KeyT>(key)),
			std::forward_as_tuple(std::forward<Args>(args)...)
		).release();

		return data.insert(result.location, newNode);
	}

	template<class KeyT, class MappedT>
	std::pair<_NodePointer, bool> _insert_or_assign(KeyT&& key, MappedT&& mapped) {
		// Insert key-value pair if key is absent, otherwise assign mapped to the value mapped by key
		const auto result = _BaseTree::_find_lower_bound(key);
		if (_BaseTree::_is_lower_bound_duplicate(result.bound, key)) {
			result.bound->value.second = std::forward<MappedT>(mapped);
			return { result.bound, false };
		}

		_BaseTree::_check_max_size();

		auto& data = this->_data;
		const _NodePointer newNode = _BSTreeTempNodeGuard<_NodeType>(
			data.head,
			std::forward<KeyT>(key),
			std::forward<MappedT>(mapped)
		).release();

		return { data.insert(result.location, newNode), true };
	}

	template<class KeyT, class MappedT>
	_NodePointer _insert_or_assign_hint(_NodePointer hintNode, KeyT&& key, MappedT&& mapped) {
		// Insert key-value pair if key is absent, otherwise assign mapped to the value mapped by key
		const auto result = _BaseTree::_find_hint(hintNode, key);
		if (result.isDuplicate) {
			result.location.parent->value.second = std::forward<MappedT>(mapped);
			return result.location.parent;
		}

		_BaseTree::_check_max_size();

		auto& data = this->_data;
		const _NodePointer newNode = _BSTreeTempNodeGuard<_NodeType>(
			data.head,
			std::forward<KeyT>(key),
			std::forward<MappedT>(mapped)
		).release();

		return data.insert(result.location, newNode);
	}

	[[noreturn]] static void _subscription_error() {
		throw std::out_of_range("Invalid subscription key!");
	}
};

template<class T1, class T2, class Comp>
void swap(Map<T1, T2, Comp>& lhs, Map<T1, T2, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T1, class T2, class Comp>
[[nodiscard]] bool operator==(const Map<T1, T2, Comp>& lhs, const Map<T1, T2, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T1, class T2, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<std::pair<const T1, T2>> operator<=>(const Map<T1, T2, Comp>& lhs, const Map<T1, T2, Comp>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}

template<class T1, class T2, class Comp = std::less<T1>>
class MultiMap : public _BSTree<
	_BSTreeMapTraits<T1, T2, Comp, _AVLTreeNode, true>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree< _BSTreeMapTraits<T1, T2, Comp, _AVLTreeNode, true>, _AVLTreeCore>;

public:
	using key_type		= T1;
	using mapped_type	= T2;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= typename _BaseTree::value_type;
	using size_type			= typename _BaseTree::size_type;
	using difference_type	= typename _BaseTree::difference_type;
	using pointer			= typename _BaseTree::pointer;
	using const_pointer		= typename _BaseTree::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseTree::iterator;
	using const_iterator	= typename _BaseTree::const_iterator;

	using reverse_iterator			= typename _BaseTree::reverse_iterator;
	using const_reverse_iterator	= typename _BaseTree::const_reverse_iterator;

	using node_handle			= typename _BaseTree::node_handle;
	using insert_return_type	= typename _BaseTree::insert_return_type;

	using _BaseTree::_BaseTree;

	template<class... Args>
	iterator emplace(Args&&... args) {
		return _BaseTree::emplace(std::forward<Args>(args)...).first;
	}

	using _BaseTree::insert;

	template<class ValueT>
	iterator insert(ValueT&& val)
		requires std::constructible_from<value_type, ValueT>
	{
		return this->emplace(std::forward<ValueT>(val));
	}

	template<class ValueT>
	iterator insert(const_iterator where, ValueT&& val)
		requires std::constructible_from<value_type, ValueT>
	{
		return _BaseTree::emplace_hint(where, std::forward<ValueT>(val));
	}

	void swap(MultiMap& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T1, class T2, class Comp>
void swap(MultiMap<T1, T2, Comp>& lhs, MultiMap<T1, T2, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T1, class T2, class Comp>
[[nodiscard]] bool operator==(const MultiMap<T1, T2, Comp>& lhs, const MultiMap<T1, T2, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T1, class T2, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<std::pair<const T1, T2>> operator<=>(const MultiMap<T1, T2, Comp>& lhs, const MultiMap<T1, T2, Comp>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompare{}
	);
}
#endif // MAP_H