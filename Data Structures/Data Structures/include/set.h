#pragma once
#ifndef SET_H
#define SET_H

#include"avl_tree.h"

template<class T, class Comp = std::less<T>>
class Set : public _AVLTree<_TreeTraits<T, T, Comp, _AVLTreeNode, false>> {

};

template<class T, class Comp = std::less<T>>
class MultiSet : public _AVLTree<_TreeTraits<T, T, Comp, _AVLTreeNode, true>> {

};
#endif // SET_H