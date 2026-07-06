type Link<T> = Box<Node<T>>;

pub struct Node<T> {
    data: T,
    next: Option<Link<T>>,
}
pub struct SimpleLinkedList<T> {
    head: Option<Link<T>>,
    tail: Option<*mut Node<T>>,
    length: usize,
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        SimpleLinkedList {
            head: None,
            tail: None,
            length: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn push(&mut self, element: T) {
        let mut new_tail = Box::new(Node {
            data: element,
            next: None,
        });
        let raw: *mut Node<T> = &mut *new_tail;

        match self.tail {
            None => self.head = Some(new_tail),
            Some(old_tail) => unsafe {
                (*old_tail).next = Some(new_tail);
            },
        }

        self.tail = Some(raw);
        self.length += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        match self.length {
            0 => None,
            1 => {
                self.tail = None;
                self.length = 0;
                return self.head.take().map(|tail| tail.data);
            }
            _ => {
                let mut curr = self.head.as_mut().unwrap();
                while let Some(ref next) = curr.next {
                    if std::ptr::eq(&(**next), self.tail.unwrap() as *const _) {
                        break;
                    }
                    curr = curr.next.as_mut().unwrap();
                }

                self.tail = Some(&mut **curr);
                self.length -= 1;
                return curr.next.take().map(|tail| tail.data);
            }
        }
    }

    pub fn peek(&self) -> Option<&T> {
        self.tail.map(|t| unsafe { &(*t).data })
    }

    #[must_use]
    pub fn rev(mut self) -> SimpleLinkedList<T> {
        let mut out = SimpleLinkedList::new();

        loop {
            let Some(data) = self.pop() else {
                break;
            };
            out.push(data);
        }

        out
    }
}

impl<T> FromIterator<T> for SimpleLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut out = SimpleLinkedList::new();

        for element in iter.into_iter() {
            out.push(element);
        }

        out
    }
}

impl<T> From<SimpleLinkedList<T>> for Vec<T> {
    fn from(mut linked_list: SimpleLinkedList<T>) -> Vec<T> {
        let mut v = Vec::with_capacity(linked_list.length);
        let mut curr = linked_list.head.take();

        while let Some(node) = curr {
            v.push(node.data);
            curr = node.next;
        }
        v
    }
}
