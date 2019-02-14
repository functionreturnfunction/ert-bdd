(describe "expect"
  (describe ":not"
    (describe ":to-be-truthy"
      (it "should fail when presented something falsey"
        (expect t :not :to-be-truthy)))

    (describe ":to-be"
      (it "sould fail when presented two things which are the same"
        (expect 'foo :not :to-be 'foo)))

    (describe ":to-equal"
      (it "should fail when presented two things which are equal"
        (expect "foo" :not :to-equal "foo")))

    (describe ":to-be-less-than"
      (it "should fail when the other number is greater"
        (expect 1 :not :to-be-less-than 2)))

    (describe ":to-be-greater-than"
      (it "should fail when the other number is lesser"
        (expect 2 :not :to-be-greater-than 1)))

    (describe ":to-have-same-items-as"
      (it "should fail when presented two lists with the same items"
        (expect '(foo) :not :to-have-same-items-as '(foo))))

    (describe ":to-match"
      (it "should fail when the regex matches the string"
        (expect "foo" :not :to-match "foo")))

    (describe ":to-contain"
      (it "should fail when the list contains the item"
        (expect '(foo) :not :to-contain 'foo)))

    (describe ":to-be-close-to"
      (it "should fail when the two numbers are within tolerance"
        (expect 1 :not :to-be-close-to 1 1))))

  (describe ":to-be-truthy"
    (it "should fail when presented something falsey"
      (expect nil :to-be-truthy)))

  (describe ":to-be"
    (it "sould fail when presented two things which are not the same"
      (expect 'foo :to-be 'bar)))

  (describe ":to-equal"
    (it "should fail when presented two things which are not equal"
      (expect "foo" :to-equal "bar")))

  (describe ":to-be-less-than"
    (it "should fail when the other number is lesser"
      (expect 2 :to-be-less-than 1)))

  (describe ":to-be-greater-than"
    (it "should fail when the other number is greater"
      (expect 1 :to-be-greater-than 2)))

  (describe ":to-have-same-items-as"
    (it "should fail when presented two lists with different items"
      (expect '(foo) :to-have-same-items-as '(bar))))

  (describe ":to-match"
    (it "should fail when the regex does not match the string"
      (expect "foo" :to-match " ")))

  (describe ":to-contain"
    (it "should fail when the list does not contain the item"
      (expect '(foo) :to-contain 'bar)))

  (describe ":to-be-close-to"
    (it "should fail when the two numbers are not within tolerance"
      (expect 100 :to-be-close-to 0 1))))
