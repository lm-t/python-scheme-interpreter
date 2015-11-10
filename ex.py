from scheme import *
first_frame = create_global_frame()
first_frame.define("x", 3)
second_frame = Frame(first_frame)
third_frame = Frame(second_frame)
fourth_frame = Frame(third_frame)
